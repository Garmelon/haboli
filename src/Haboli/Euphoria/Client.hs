{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}

module Haboli.Euphoria.Client
  (
  -- * The Client monad
    Client
  , runClient
  , ConnectionDetails(..)
  , defaultDetails
  -- ** Getters
  , getHost
  , getPort
  , getPath
  -- ** Event handling
  , Event(..)
  , nextEvent
  , respondingToPing
  -- ** Exception handling
  , ClientException(..)
  , Haboli.Euphoria.Client.throw
  -- ** Euphoria commands
  -- *** Session commands
  , pingReply
  -- *** Chat room commands
  , nick
  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy       as BS
import           Data.Foldable
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import qualified Data.Text                  as T
import           Data.Time
import           Network.Socket
import qualified Network.WebSockets         as WS
import qualified Wuss                       as WSS

import           Haboli.Euphoria.Api

--TODO: Add all the events
data Event
  = EventPing PingEvent
  | EventSnapshot SnapshotEvent
  | PlaceholderEvent --TODO: remove this event
  deriving (Show)

instance FromJSON Event where
  parseJSON v = foldr (<|>) mempty
    [ EventPing <$> parseJSON v
    , EventSnapshot <$> parseJSON v
    ]

--TODO: Add more exceptions for other things that can also go wrong (parsing, connection already closed, ...)
data ClientException e
  = ServerException (Maybe T.Text) (Maybe T.Text)
  -- ^ @'ServerError' error throttled@ is an error sent by the server in
  -- response to a command. @error@ is a message that appears if a command
  -- fails. @throttled@ is a message that appears if the client should slow down
  -- its command rate.
  | StoppedException
  | DecodeException T.Text
  -- ^ At some point during decoding a websocket packet, something went wrong.
  | CustomException e
  deriving (Show)

instance FromJSON (ClientException e) where
  parseJSON (Object o) = do
    serverError <- o .:? "error"
    isThrottled <- o .:? "throttled" .!= False
    throttledReason <- o .:? "throttled_reason"
    let throttled = if isThrottled then Just (fromMaybe "" throttledReason) else Nothing
    when (isNothing serverError && isNothing throttled) $
      fail "there is no error and the client is not throttled"
    pure $ ServerException serverError throttled
  parseJSON v = typeMismatch "Object" v

-- | This type is used by the websocket thread to send the server's replies to
-- the client. Since exceptions like a 'ServerError' may occur, they are
-- explicitly included in the type stored in the 'MVar'.
--
-- The fancy types are there so I don't have to explicitly specify the response
-- in some sum type or similar.
data AwaitingReply e
  = forall r. FromJSON r => AwaitingReply (TMVar (Either (ClientException e) r))

-- | A 'Map.Map' of empty 'TMVar's waiting for their respective reply packet
-- from the server.
type AwaitingReplies e = Map.Map T.Text (AwaitingReply e)

data ClientInfo e = ClientInfo
  { ciDetails    :: ConnectionDetails
  , ciConnection :: WS.Connection
  , ciAwaiting   :: TVar (AwaitingReplies e)
  , ciEventChan  :: TChan Event
  , ciPacketId   :: TVar Integer
  , ciStopped    :: TVar Bool -- only modified by websocket thread
  }

-- This type declaration feels lispy in its parenthesisness
newtype Client e a = Client (ReaderT (ClientInfo e)
                             (ExceptT (ClientException e)
                              IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

{- The websocket listening thread -}

--TODO: This could close the ws connection and stop the client instead
-- | An exception handler that ignores messages that could not be decoded
-- properly. It only prints the exceptions via 'putStrLn'.
ignoringInvalidMessages :: WS.ConnectionException -> IO ()
ignoringInvalidMessages (WS.ParseException message)   = putStrLn $ "ParseException: " ++ message
ignoringInvalidMessages (WS.UnicodeException message) = putStrLn $ "UnicodeException: " ++ message
ignoringInvalidMessages e = throwIO e

-- | An exception handler that stops the client if any sort of
-- 'WS.ConnectionException' occurs. It does this by setting 'ciStopped' to True
-- and cancelling all 'AwaitingReply'-s in 'ciAwaiting'.
cancellingAllReplies :: ClientInfo e -> WS.ConnectionException -> IO ()
cancellingAllReplies info _ = atomically $ do
  writeTVar (ciStopped info) True
  -- Cancel all replies
  replyMap <- readTVar (ciAwaiting info)
  for_ replyMap $ \(AwaitingReply v) ->
    putTMVar v (Left StoppedException)

parseAndSendEvent :: BS.ByteString -> TChan Event -> IO ()
parseAndSendEvent msg eventChan =
  for_ (decode msg) $ \event ->
    atomically $ writeTChan eventChan event

parseAndSendReply :: BS.ByteString -> TVar (AwaitingReplies e) -> IO ()
parseAndSendReply msg awaiting = do
  let maybePacketId = parseMaybe parsePacketId =<< decode msg
  for_ maybePacketId $ \packetId -> atomically $ do
    awaitingMap <- readTVar awaiting
    for_ (awaitingMap Map.!? packetId) $ \(AwaitingReply replyVar) -> do
      let maybeExceptionOrReply = (Left <$> decode msg) <|> (Right <$> decode msg)
          invalidStructureException = Left $ DecodeException "invalid message json structure"
      putTMVar replyVar $ fromMaybe invalidStructureException maybeExceptionOrReply
      modifyTVar awaiting $ Map.delete packetId
  where
    parsePacketId :: Value -> Parser T.Text
    parsePacketId (Object o) = o .: "id"
    parsePacketId v          = typeMismatch "Object" v

runWebsocketThread :: ClientInfo e -> IO ()
runWebsocketThread info
  = WS.withPingThread (ciConnection info) pingInterval (pure ())
  $ handle (cancellingAllReplies info) $ forever
  $ handle ignoringInvalidMessages $ do
    msg <- WS.receiveData (ciConnection info)
    -- print msg
    parseAndSendEvent msg (ciEventChan info)
    parseAndSendReply msg (ciAwaiting info)
  where
    pingInterval = cdPingInterval $ ciDetails info

{- Running the Client monad -}

data ConnectionDetails = ConnectionDetails
  { cdHost         :: HostName
  , cdPort         :: PortNumber
  , cdPath         :: String
  , cdPingInterval :: Int
  } deriving (Show)

defaultDetails :: ConnectionDetails
defaultDetails = ConnectionDetails
  { cdHost = "euphoria.io"
  , cdPort = 443
  , cdPath = "/room/test/ws"
  , cdPingInterval = 10
  }

--TODO: Close connection after client finishes running if it hasn't already been closed
runClient :: ConnectionDetails -> Client e a -> IO (Either (ClientException e) a)
runClient details (Client stack)
  = withSocketsDo
  $ WSS.runSecureClient (cdHost details) (cdPort details) (cdPath details)
  $ \connection -> do
    awaiting <- newTVarIO Map.empty
    eventChan <- newTChanIO
    packetId <- newTVarIO 0
    stopped <- newTVarIO False
    let info = ClientInfo
          { ciDetails = details
          , ciConnection = connection
          , ciAwaiting = awaiting
          , ciEventChan = eventChan
          , ciPacketId = packetId
          , ciStopped = stopped
          }
    -- Start the websocket thread, which will notify this thread when it stops
    wsThreadFinished <- newEmptyMVar
    void $ forkFinally (runWebsocketThread info) (\_ -> putMVar wsThreadFinished ())
    -- Run the actual 'Client' in this thread
    result <- runExceptT $ runReaderT stack info
    -- Close the connection if it is not already closed, and wait until the
    -- websocket thread stops
    handle ignoreAllExceptions $ WS.sendClose connection $ T.pack "Goodbye :D"
    takeMVar wsThreadFinished
    pure result
  where
    ignoreAllExceptions :: WS.ConnectionException -> IO ()
    ignoreAllExceptions _ = pure ()

{- Private operations -}

throwRaw :: ClientException e -> Client e a
throwRaw e = Client $ lift $ throwE e

getClientInfo :: Client e (ClientInfo e)
getClientInfo = Client ask

newPacketId :: Client e T.Text
newPacketId = do
  packetIdTVar <- ciPacketId <$> getClientInfo
  liftIO $ atomically $ do
    currentId <- readTVar packetIdTVar
    modifyTVar packetIdTVar (+1)
    pure $ T.pack $ show currentId

-- | Attempt to send a message via the websocket connection, catching and
-- re-throwing all relevant exceptions inside the 'Client' monad.
safeSend :: ToJSON a => WS.Connection -> a -> Client e ()
safeSend connection packet = do
  result <- liftIO
    $ handle convertToException
    $ Nothing <$ WS.sendTextData connection (encode packet)
  case result of
    Nothing -> pure ()
    Just e  -> throwRaw e
  where
    convertToException :: WS.ConnectionException -> IO (Maybe (ClientException e))
    convertToException (WS.CloseRequest _ _)         = pure $ Just StoppedException
    convertToException WS.ConnectionClosed           = pure $ Just StoppedException
    convertToException (WS.ParseException message)   = pure $ Just $ DecodeException
      $ "could not parse websockets stream, client sent garbage: " <> T.pack message
    convertToException (WS.UnicodeException message) = pure $ Just $ DecodeException
      $ "could not decode unicode: " <> T.pack message

-- | Send a packet and automatically add a packet id
sendPacket :: ToJSONObject o => o -> Client e T.Text
sendPacket packet = do
  connection <- ciConnection <$> getClientInfo
  -- No need to check if 'ciStopped' is True because 'WS.sendTextData' will
  -- throw an exception anyways.
  packetId <- newPacketId
  let packetWithId = toJSONObject packet <> ("id" .= packetId)
  safeSend connection packetWithId
  pure packetId

-- | Send a packet and wait for a reply from the server.
sendPacketWithReply :: (ToJSONObject o, FromJSON r) => o -> Client e r
sendPacketWithReply packet = do
  info <- getClientInfo
  packetId <- sendPacket packet
  -- Create and insert a new empty TMVar into the AwaitingReplies map
  maybeReplyVar <- liftIO $ atomically $ do
    stopped <- readTVar $ ciStopped info
    if stopped
      then pure Nothing
      else do
        replyVar <- newEmptyTMVar
        modifyTVar (ciAwaiting info) $ Map.insert packetId (AwaitingReply replyVar)
        pure $ Just replyVar
  case maybeReplyVar of
    Nothing -> throwRaw StoppedException
    Just replyVar -> do
      reply <- liftIO $ atomically $ readTMVar replyVar
      case reply of
        Left e  -> throwRaw e
        Right r -> pure r

{- Public operations -}

{- Getters -}

getHost :: Client e HostName
getHost = cdHost . ciDetails <$> getClientInfo

getPort :: Client e PortNumber
getPort = cdPort . ciDetails <$> getClientInfo

getPath :: Client e String
getPath = cdPath . ciDetails <$> getClientInfo

{- Special operations -}

nextEvent :: Client e Event
nextEvent = do
  info <- getClientInfo
  exceptionOrEvent <- liftIO $ atomically $ do
    stopped <- readTVar (ciStopped info)
    if stopped
      then pure $ Left StoppedException
      else Right <$> readTChan (ciEventChan info)
  case exceptionOrEvent of
    Left e  -> throwRaw e
    Right e -> pure e

respondingToPing :: Client e Event -> Client e Event
respondingToPing holdingEvent = do
  event <- holdingEvent
  case event of
    EventPing e -> pingReply (pingTime e)
    _           -> pure ()
  pure event

{- Exception handling -}

--TODO: Add more commands and find more appropriate names

throw :: e -> Client e a
throw = throwRaw . CustomException

{- Euphoria commands -}

{- Session commands -}

pingReply :: UTCTime -> Client e ()
pingReply = void . sendPacket . PingReply

{- Chat room commands -}

nick :: T.Text -> Client e T.Text
nick targetNick = do
  reply <- sendPacketWithReply $ NickCommand targetNick
  pure $ nickReplyTo reply

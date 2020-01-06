{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module Haboli.Euphoria.Client where

import Control.Exception
import           Control.Concurrent
import           Control.Concurrent.Chan
import Data.Foldable
import Control.Monad.Trans.Class
import Data.Traversable
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import           Data.Aeson
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Time
import           Network.Socket
import qualified Network.WebSockets         as WS
import qualified Wuss                       as WSS

import           Haboli.Euphoria.Api

--TODO: Add all the events
-- | An event sent by the server. See
-- <http://api.euphoria.io/#asynchronous-events>.
data ServerEvent
  = ServerHello HelloEvent
  | ServerSnapshot SnapshotEvent
  deriving (Show)

-- | An event coming from the connection to the server.
data Event
  = EventServer ServerEvent
  -- ^ The server has sent an event.
  | EventStopped
  -- ^ The connection has been closed. This event is always the last event and
  -- after this event, no other event will come from the connection.
  deriving (Show)

--TODO: Decide between Exception and Error
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
  | CustomError e
  deriving (Show)

-- | This type is used by the websocket thread to send the server's replies to
-- the client. Since exceptions like a 'ServerError' may occur, they are
-- explicitly included in the type stored in the 'MVar'.
--
-- The fancy types are there so I don't have to explicitly specify the response
-- in some sum type or similar.
newtype AwaitingReply e
  = AwaitingReply (forall r. FromJSON r => Either (ClientException e) r)

-- | A 'Map.Map' of empty 'TMVar's waiting for their respective reply packet
-- from the server.
type AwaitingReplies e = Map.Map T.Text (TMVar (AwaitingReply e))

data ClientInfo e = ClientInfo
            { ciDetails :: ConnectionDetails
            , ciConnection        :: WS.Connection
            , ciPacketId          :: TVar Integer
            , ciWsThreadId        :: ThreadId
            , ciAwaiting          :: TVar (AwaitingReplies e)
            , ciEventChan         :: Chan Event
            , ciStopped           :: TVar Bool -- only modified by websocket thread
            }

-- This type declaration feels lispy in its parenthesisness
newtype Client e a = Client (ReaderT (ClientInfo e)
                             (ExceptT (ClientException e)
                              IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

{- The websocket listening thread -}

ignoringInvalidMessages :: WS.ConnectionException -> IO ()
ignoringInvalidMessages (WS.ParseException message)   = putStrLn $ "ParseException: " ++ message
ignoringInvalidMessages (WS.UnicodeException message) = putStrLn $ "UnicodeException: " ++ message
ignoringInvalidMessages e = throwIO e

cancelAllReplies :: TVar (AwaitingReplies e) -> STM ()
cancelAllReplies awaiting = do
  replyMap <- readTVar awaiting
  for_ replyMap $ \v ->
    putTMVar v (AwaitingReply (Left StoppedException))

wsThread :: WS.Connection -> Chan Event -> TVar (AwaitingReplies e) -> TVar Bool -> IO ()
wsThread connection eventChan awaiting stopped
  = handle stopHandler
  $ forever
  $ handle ignoringInvalidMessages
  $ do
    msg <- WS.receiveData connection
    --TODO: Actually parse the stuff and send it to the event channel
    T.putStrLn msg
  where
    stopHandler :: WS.ConnectionException -> IO ()
    stopHandler _ = do
      -- After 'stopped' is set to True, 'awaiting' is not modified by any
      -- thread. Because of this, the call to 'cancelAllReplies' wouldn't need
      -- to happen atomically with setting 'stopped' to True, but I still do it
      -- atomically.
      atomically $ writeTVar stopped True >> cancelAllReplies awaiting
      writeChan eventChan EventStopped

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

runClient :: ConnectionDetails -> Client e a -> IO (Either (ClientException e) a)
runClient details (Client stack)
  = withSocketsDo
  $ WSS.runSecureClient (cdHost details) (cdPort details) (cdPath details)
  $ \connection -> do
    packetId <- newTVarIO 0
    awaiting <- newTVarIO Map.empty
    eventChan <- newChan
    stopped <- newTVarIO False

    wsThreadId <- forkIO
      $ WS.withPingThread connection (cdPingInterval details) (pure ())
      $ wsThread connection eventChan awaiting stopped

    let info = ClientInfo
          { ciDetails = details
          , ciConnection = connection
          , ciPacketId = packetId
          , ciWsThreadId = wsThreadId
          , ciAwaiting = awaiting
          , ciEventChan = eventChan
          , ciStopped = stopped
          }
    runExceptT $ runReaderT stack info

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
sendPacket :: Object -> Client e T.Text
sendPacket packet = do
  connection <- ciConnection <$> getClientInfo
  -- No need to check if 'ciStopped' is True because 'WS.sendTextData' will
  -- throw an exception anyways.
  packetId <- newPacketId
  let packetWithId = packet <> ("id" .= packetId)
  safeSend connection packetWithId
  pure packetId

-- | Send a packet and wait for a reply from the server.
sendPacketWithReply :: FromJSON r => Object -> Client e r
sendPacketWithReply packet = do
  info <- getClientInfo
  packetId <- sendPacket packet
  maybeReplyVar <- liftIO $ atomically $ do
    stopped <- readTVar $ ciStopped info
    if stopped
      then pure Nothing
      else do
        replyVar <- newEmptyTMVar
        modifyTVar (ciAwaiting info) (Map.insert packetId replyVar)
        pure $ Just replyVar
  case maybeReplyVar of
    Nothing -> throwRaw StoppedException
    Just replyVar -> do
      (AwaitingReply reply) <- liftIO $ atomically $ do
        reply <- readTMVar replyVar
        modifyTVar (ciAwaiting info) (Map.delete packetId)
        pure reply
      case reply of
        Left e -> throwRaw e
        Right r -> pure r

{- Public operations -}

getHost :: Client e HostName
getHost = cdHost . ciDetails <$> getClientInfo

getPort :: Client e PortNumber
getPort = cdPort . ciDetails <$> getClientInfo

getPath :: Client e String
getPath = cdPath . ciDetails <$> getClientInfo

stop :: Client e ()
stop = do
  ci <- getClientInfo
  liftIO $ WS.sendClose (ciConnection ci) $ T.pack "Goodbye :D"

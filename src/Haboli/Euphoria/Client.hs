{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}

-- | This library is based around the custom 'Client' monad. It is based on 'IO'
-- and represents computations that happen while a connection to an euphoria
-- server is open. Once a 'Client' finishes executing, the connection is
-- automatically closed. If the connection closes unexpectedly while the
-- corresponding 'Client' is still running, it is notified and commands like
-- 'send' or 'nick' will result in an exception. The 'Client' does not
-- automatically reconnect.
--
-- The 'Client' monad supports exceptions via the 'throw', 'catch' and 'handle'
-- operations, as well as multiple threads via the 'fork' and 'wait' operations.
-- It supports all session and chat room commands listed in <api.euphoria.io>.

module Haboli.Euphoria.Client
  (
  -- * The Client monad
    Client
  , runClient
  , ConnectionConfig(..)
  , defaultConfig
  , withRoom
  , getConnectionConfig
  -- ** Event handling
  , Event(..)
  , nextEvent
  -- ** Exception handling
  , ClientException(..)
  , throw
  , catch
  , handle
  -- ** Threading
  , Thread
  , fork
  , wait
  -- ** Euphoria commands
  -- *** Session commands
  , auth
  , ping
  , pingReply
  -- *** Chat room commands
  , getMessage
  , getLog
  , getLogBefore
  , nick
  , initiatePm
  , send
  , reply
  , reply'
  , who
  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception          as E
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Foldable
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import qualified Data.Text                  as T
import           Data.Time
import qualified Network.Socket             as S
import qualified Network.WebSockets         as WS
import qualified Wuss                       as WSS

import           Haboli.Euphoria.Api

-- | This type represents a @Reply e r@ with arbitrary @r@ that has yet to be
-- received. The @forall@ allows whoever creates the AwaitingReply to decide on
-- the type of @r@.
data AwaitingReply e
  = forall r. FromJSON r => AwaitingReply (TMVar (Reply e r))

-- | A 'Map.Map' of empty 'TMVar's waiting for their respective reply packet
-- from the server.
type AwaitingReplies e = Map.Map T.Text (AwaitingReply e)

data ClientInfo e = ClientInfo
  { ciDetails    :: ConnectionConfig
  , ciConnection :: WS.Connection
  , ciAwaiting   :: TVar (AwaitingReplies e)
  , ciEventChan  :: TChan Event
  , ciPacketId   :: TVar Integer
  , ciStopped    :: TVar Bool -- only modified by websocket thread
  }

-- This type declaration feels lispy in its parenthesisness
-- | @'Client' e a@ is the monad in which clients are written. @e@ is the custom
-- exception type that can be seen in functions like 'catch' or 'runClient'. @a@
-- is the monad return type.
--
-- A value of type @'Client' e a@ can be thought of as an action returning a
-- value of type @a@ or throwing an exception of type @'ClientException' e@ that
-- can be executed in a client (which usually has an open websocket connection)
--
-- For more information on how a 'Client' is executed, see the documentation of
-- 'runClient'.
newtype Client e a = Client (ExceptT (ClientException e)
                             (ReaderT (ClientInfo e)
                              IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

{- The websocket listening thread -}

-- | Close a 'WS.Connection', catching and ignoring any
-- 'WS.ConnectionException's in the process.
safelyCloseConnection :: WS.Connection -> IO ()
safelyCloseConnection connection =
  E.handle ignoreAllExceptions $
  WS.sendClose connection $ T.pack "Goodbye :D"
  where
    ignoreAllExceptions :: WS.ConnectionException -> IO ()
    ignoreAllExceptions _ = pure ()

-- | An exception handler that closes the 'WS.Connection' when it receives an
-- invalidly formatted message from the server.
closeConnectionOnInvalidMessage :: WS.Connection -> WS.ConnectionException -> IO ()
closeConnectionOnInvalidMessage connection (WS.ParseException _) =
  safelyCloseConnection connection
closeConnectionOnInvalidMessage connection (WS.UnicodeException _) =
  safelyCloseConnection connection
closeConnectionOnInvalidMessage _ e = E.throwIO e

-- | An exception handler that stops the client if any sort of
-- 'WS.ConnectionException' occurs. It does this by setting ciStopped to True
-- and cancelling all AwaitingReply-s in ciAwaiting.
cancelAllReplies :: ClientInfo e -> WS.ConnectionException -> IO ()
cancelAllReplies info _ = atomically $ do
  writeTVar (ciStopped info) True
  -- Cancel all replies
  replyMap <- readTVar (ciAwaiting info)
  for_ replyMap $ \(AwaitingReply v) ->
    putTMVar v $ emptyReply $ Left StoppedException

parseAndSendEvent :: Value -> TChan Event -> IO ()
parseAndSendEvent v eventChan =
  for_ (fromJSON v) $ \event ->
    atomically $ writeTChan eventChan event

parseAndSendReply :: Value -> TVar (AwaitingReplies e) -> IO ()
parseAndSendReply v awaiting = do
  -- Since the client is stopped when the websocket thread finishes, and this
  -- function is called inside the websocket thread, from the point of view of
  -- this function, the client is never stopped. Because of that, we don't have
  -- to check 'ciStopped' because we know the client isn't stopped.
  let maybePacketId = parseMaybe (parseJSON >=> (.: "id")) v
  for_ maybePacketId $ \packetId -> atomically $ do
    awaitingMap <- readTVar awaiting
    for_ (awaitingMap Map.!? packetId) $ \(AwaitingReply replyVar) -> do
      putTMVar replyVar $ fromMaybe invalidStructureException $ parseMaybe parseJSON v
      modifyTVar awaiting $ Map.delete packetId
  where
    invalidStructureException :: Reply e r
    invalidStructureException =
      emptyReply $ Left $ DecodeException "invalid message json structure"

runWebsocketThread :: ClientInfo e -> IO ()
runWebsocketThread info =
  WS.withPingThread connection pingInterval (pure ()) $
  -- Stop the client and cancel all replies before this thread finishes
  E.handle (cancelAllReplies info) $
  forever $
  -- If the client receives an invalidly formatted message, be careful and just
  -- disconnect because something went really wrong
  E.handle (closeConnectionOnInvalidMessage connection) $ do
    msg <- WS.receiveData connection
    case decode msg of
      -- If the client receives invalid JSON, also disconnect for the same reason
      -- as above
      Nothing -> safelyCloseConnection connection
      Just value -> do
        parseAndSendEvent value (ciEventChan info)
        parseAndSendReply value (ciAwaiting info)
  where
    connection = ciConnection info
    pingInterval = confPingInterval $ ciDetails info

{- Running the Client monad -}

-- | Configuration for the websocket connection. The websocket connection always
-- uses https.
data ConnectionConfig = ConnectionConfig
  { confHost         :: S.HostName
  , confPort         :: S.PortNumber
  , confPath         :: String
  , confPingInterval :: Int
  } deriving (Show)

-- | A default configuration that points the bot to the room @&test@ at
-- <https://euphoria.io/room/test>.
defaultConfig :: ConnectionConfig
defaultConfig = ConnectionConfig
  { confHost = "euphoria.io"
  , confPort = 443
  , confPath = "/room/test/ws"
  , confPingInterval = 10
  }

-- | @'withRoom' roomname config@ modifies the 'confPath' of @config@ to point
-- to the room @roomname@.
withRoom :: String -> ConnectionConfig -> ConnectionConfig
withRoom room config = config{confPath = "/room/" ++ room ++ "/ws"}

--TODO: Catch IO exceptions that occur when a connection could not be created
-- | Execute a 'Client'.
--
-- The execution is bound to a single websocket connection. Once that connection
-- closes for any reason, calls to 'nextEvent' or to commands throw a
-- 'StoppedException'. If the 'Client' finishes executing, the websocket
-- connection is closed automatically.
--
-- At the moment, IO exceptions that occur while creating the websocket
-- connection are not caught. This will probably change soon.
runClient :: ConnectionConfig -> Client e a -> IO (Either (ClientException e) a)
runClient details (Client stack) =
  S.withSocketsDo $
  WSS.runSecureClient (confHost details) (confPort details) (confPath details) $ \connection -> do
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
    void $
      forkFinally (runWebsocketThread info) (\_ -> putMVar wsThreadFinished ())
    -- Run the actual 'Client' in this thread
    result <- runReaderT (runExceptT stack) info
    -- Close the connection and wait until the websocket thread stops
    safelyCloseConnection connection
    takeMVar wsThreadFinished
    pure result

{- Getters -}

getClientInfo :: Client e (ClientInfo e)
getClientInfo = Client $ lift ask

-- | Get the 'ConnectionConfig' the current websocket connection uses.
getConnectionConfig :: Client e ConnectionConfig
getConnectionConfig = ciDetails <$> getClientInfo

{- Event handling -}

-- | This type represents events sent by the server. For more information on the
-- specific events, see their documentation in "Haboli.Euphoria.Api" or have a
-- look at <http://api.euphoria.io/#asynchronous-events>.
data Event
  = EventBounce BounceEvent
  | EventDisconnect DisconnectEvent
  | EventHello HelloEvent
  | EventJoin JoinEvent
  | EventLogin LoginEvent
  | EventLogout LogoutEvent
  | EventNetwork NetworkEvent
  | EventNick NickEvent
  | EventEditMessage EditMessageEvent
  | EventPart PartEvent
  | EventPing PingEvent
  | EventPmInitiate PmInitiateEvent
  | EventSend SendEvent
  | EventSnapshot SnapshotEvent
  deriving (Show)

instance FromJSON Event where
  parseJSON v = foldr (<|>) mempty
    [ EventBounce <$> parseJSON v
    , EventDisconnect <$> parseJSON v
    , EventHello <$> parseJSON v
    , EventJoin <$> parseJSON v
    , EventLogin <$> parseJSON v
    , EventLogout <$> parseJSON v
    , EventNetwork <$> parseJSON v
    , EventNick <$> parseJSON v
    , EventEditMessage <$> parseJSON v
    , EventPart <$> parseJSON v
    , EventPing <$> parseJSON v
    , EventPmInitiate <$> parseJSON v
    , EventSend <$> parseJSON v
    , EventSnapshot <$> parseJSON v
    ]

-- | Retrieve the next event sent by the server. This function blocks until a
-- new event is available or the connection closes, in which case it will throw
-- a 'StoppedException'.
nextEvent :: Client e Event
nextEvent = do
  info <- getClientInfo
  -- This appears to stop correctly when 'ciStopped' is set to True, even if
  -- that happens from a different thread while this thread is waiting for the
  -- event channel.
  exceptionOrEvent <- liftIO $ atomically $ do
    stopped <- readTVar (ciStopped info)
    if stopped
      then pure $ Left StoppedException
      else Right <$> readTChan (ciEventChan info)
  case exceptionOrEvent of
    Left e  -> throwRaw e
    Right e -> pure e

{- Exception handling -}

-- | The type of exceptions in the 'Client' monad.
data ClientException e
  = ServerException T.Text
  -- ^ The server has sent an error message as a reply to a command. Usually
  -- this happens if a command is used incorrectly.
  | StoppedException
  -- ^ The websocket connection underlying the 'Client' was closed.
  | DecodeException T.Text
  -- ^ At some point during decoding a websocket packet, something went wrong.
  | UnexpectedException E.SomeException
  -- ^ While a forked thread was executed, an unexpected exception was thrown in
  -- the IO monad.
  | CustomException e
  -- ^ A custom exception was thrown via 'throw'.
  deriving (Show)

throwRaw :: ClientException e -> Client e a
throwRaw = Client . throwE

-- | Throw a 'CustomException'.
throw :: e -> Client e a
throw = throwRaw . CustomException

-- | Catch a 'ClientException'. This method works like 'catchE'.
catch :: Client e a -> (ClientException e -> Client e a) -> Client e a
-- The main reason why the 'ExceptT' is wrapped around the 'ReaderT' in the
-- 'Client' monad is that it makes this function easier to implement
catch c f = Client $ catchE (unclient c) (unclient . f)
  where
    unclient (Client m) = m

-- | A version of 'catch' with its arguments flipped. It is named after
-- 'E.handle'.
handle :: (ClientException e -> Client e a) -> Client e a -> Client e a
handle = flip Haboli.Euphoria.Client.catch

{- Threading -}

-- | This type represents a thread spawned by 'fork'.
newtype Thread e a = Thread (MVar (Either (ClientException e) a))

-- | @'fork' p@ forks a new thread running the 'Client' @p@. To wait for the
-- thread to finish executing and collect the result, use 'wait'.
fork :: Client e a -> Client e (Thread e a)
fork (Client f) = do
  info <- getClientInfo
  waitVar <- liftIO newEmptyMVar
  let thread = runReaderT (runExceptT f) info
      andThen (Left e)  = putMVar waitVar $ Left $ UnexpectedException e
      andThen (Right r) = putMVar waitVar r
  void $ liftIO $ forkFinally thread andThen
  pure $ Thread waitVar

-- | Wait for a thread to finish executing and collect the result. If the thread
-- threw a 'ClientException', that exception is rethrown. If the thread threw an
-- IO exception, that exception is wrapped in an 'UnexpectedException' and
-- thrown.
wait :: Thread e a -> Client e a
wait (Thread waitVar) = do
  result <- liftIO $ readMVar waitVar
  case result of
    (Left e)  -> throwRaw e
    (Right a) -> pure a

{- Euphoria commands -}

-- | A server's reply to a command.
data Reply e r = Reply
  { replyThrottled :: Maybe T.Text
  , replyResult    :: Either (ClientException e) r
  }
  deriving (Show)

instance FromJSON r => FromJSON (Reply e r) where
  parseJSON v@(Object o) = Reply
    <$> throttledParser
    <*> ((Left <$> errorParser) <|> (Right <$> parseJSON v))
    where
      -- I don't know if the API guarantees that there is always a
      -- "throttled_reason" if the commands are throttled or not. For now, I'm
      -- trusting the "throttled" boolean more than the "throttled_reason."
      throttledParser = do
        throttled <- o .:? "throttled" .!= False
        if throttled
          then Just <$> o .:? "throttled_reason" .!= ""
          else pure Nothing
      errorParser = ServerException <$> o .: "error"
  parseJSON v = typeMismatch "Object" v

emptyReply :: Either (ClientException e) r -> Reply e r
emptyReply = Reply Nothing

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
    $ E.handle convertToException
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
      answer <- liftIO $ atomically $ readTMVar replyVar
      case replyResult answer of
        Left e  -> throwRaw e
        Right r -> pure r

{- Session commands -}

-- | Try to authenticate with a password and return the error reason if it
-- *doesn't* succeed.
auth :: T.Text -> Client e (Either T.Text ())
auth password = do
  answer <- sendPacketWithReply $ AuthWithPasscode password
  pure $ case answer of
    AuthSuccessful    -> Right ()
    AuthFailed reason -> Left reason

-- | Send a ping (not a websocket ping) to the server, who has to reply with the
-- same time stamp.
ping :: UTCTime -> Client e UTCTime
ping time = do
  PingReply time' <- sendPacketWithReply $ PingCommand time
  pure time'

-- | Send a reply to a 'PingEvent' sent by the server.
pingReply :: UTCTime -> Client e ()
pingReply = void . sendPacket . PingReply

{- Chat room commands -}

-- | Retrieve the full content of a single message in the room.
getMessage :: Snowflake -> Client e Message
getMessage mId = do
  GetMessageReply msg <- sendPacketWithReply $ GetMessageCommand mId
  pure msg

-- | @'getLog' n@ requests @n@ messages from the room's log, starting with the
-- latest message and going backwards by the send time/message id.
getLog :: Int -> Client e [Message]
getLog n = do
  LogReply msgs _ <- sendPacketWithReply $ LogCommand n Nothing
  pure msgs

-- | @'getLogBefore' n before@ works similar to @'getLog' n@, but it requests
-- messages starting with the latest message before the message with id
-- @before@.
getLogBefore :: Int -> Snowflake -> Client e [Message]
getLogBefore n before = do
  LogReply msgs _ <- sendPacketWithReply $ LogCommand n (Just before)
  pure msgs

-- | Initiate a PM with another user. Returns a tuple @(pmId, toNick)@, where
-- @pmId@ is the id of the PM room, and @toNick@ is the nick of the user the
-- request was sent to.
initiatePm :: UserId -> Client e (Snowflake, T.Text)
initiatePm uId = do
  PmInitiateReply pmId toNick <- sendPacketWithReply $ PmInitiateCommand uId
  pure (pmId, toNick)

-- | Change your own nick. Returns a tuple @(oldNick, newNick)@.
nick :: T.Text -> Client e (T.Text, T.Text)
nick targetNick = do
  answer <- sendPacketWithReply $ NickCommand targetNick
  pure (nickReplyFrom answer, nickReplyTo answer)

-- | Send a new top-level message. Returns the sent message.
send :: T.Text -> Client e Message
send content = do
  SendReply msg <- sendPacketWithReply $ SendCommand content Nothing
  pure msg

-- | Reply to a message via its id. Returns the sent message.
reply' :: Snowflake -> T.Text -> Client e Message
reply' messageId content = do
  (SendReply msg) <- sendPacketWithReply $ SendCommand content (Just messageId)
  pure msg

-- | Reply to a message. Returns the sent message.
--
-- This function is equivalent to @'reply'' . 'msgId'@.
reply :: Message -> T.Text -> Client e Message
reply = reply' . msgId

-- | See who is currently connected to the room.
who :: Client e [SessionView]
who = do
  WhoReply sessions <- sendPacketWithReply WhoCommand
  pure sessions

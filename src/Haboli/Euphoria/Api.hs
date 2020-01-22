{-# LANGUAGE OverloadedStrings #-}

-- | This module attempts to map the structure of the ephoria API to types.

module Haboli.Euphoria.Api
  ( ToJSONObject(..)
  -- * Basic types
  , AuthOption(..)
  , Message(..)
  , PersonalAccountView(..)
  , SessionView(..)
  , Snowflake
  , UserType(..)
  , UserId(..)
  -- * Asynchronous events
  -- ** bounce-event
  , BounceEvent(..)
  -- ** disconnect-event
  , DisconnectEvent(..)
  -- ** hello-event
  , HelloEvent(..)
  -- ** join-event
  , JoinEvent(..)
  -- ** login-event
  , LoginEvent(..)
  -- ** logout-event
  , LogoutEvent(..)
  -- ** network-event
  , NetworkEvent(..)
  -- ** nick-event
  , NickEvent(..)
  -- ** edit-message-event
  , EditMessageEvent(..)
  -- ** part-event
  , PartEvent(..)
  -- ** ping-event
  , PingEvent(..)
  -- ** pm-initiate-event
  , PmInitiateEvent(..)
  -- ** send-event
  , SendEvent(..)
  -- ** snapshot-event
  , SnapshotEvent(..)
  -- * Session commands
  -- ** auth
  , AuthCommand(..)
  , AuthReply(..)
  -- ** ping
  , PingCommand(..)
  , PingReply(..)
  -- * Chat room commands
  -- ** get-message
  , GetMessageCommand(..)
  , GetMessageReply(..)
  -- ** log
  , LogCommand(..)
  , LogReply(..)
  -- ** nick
  , NickCommand(..)
  , NickReply(..)
  -- ** pm-initiate
  , PmInitiateCommand(..)
  , PmInitiateReply(..)
  -- ** send
  , SendCommand(..)
  , SendReply(..)
  -- ** who
  , WhoCommand(..)
  , WhoReply(..)
  ) where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict   as HMap
import qualified Data.Text             as T
import           Data.Time
import           Data.Time.Clock.POSIX

-- | A class for all types that can be converted into an
-- 'Data.Aeson.Types.Object'. Similar to 'ToJSON', but more restrictive.
class ToJSONObject a where
  toJSONObject :: a -> Object

fromPacket :: T.Text -> (Object -> Parser a) -> Value -> Parser a
fromPacket packetType parser v = parseJSON v >>= \o -> do
  actualType <- o .: "type"
  when (actualType /= packetType) $
    fail $ T.unpack $ "packet type is not " <> packetType
  packetData <- o .: "data"
  parser packetData

toPacket :: T.Text -> Value -> Object
toPacket packetType packetData = HMap.fromList
  [ "type" .= packetType
  , "data" .= packetData
  ]

{- Basic types -}

-- | A method of authenticating.
data AuthOption = Passcode
  deriving (Show, Eq)

instance ToJSON AuthOption where
  toJSON Passcode = String "passcode"

instance FromJSON AuthOption where
  parseJSON (String "passcode") = pure Passcode
  parseJSON (String _)          = fail "invalid value"
  parseJSON v                   = typeMismatch "String" v

-- | A 'Message' is a node in a room’s log. It corresponds to a chat message, or
-- a post, or any broadcasted event in a room that should appear in the log. See
-- <http://api.euphoria.io/#message>.
data Message = Message
  { msgId              :: Snowflake
  , msgParent          :: Maybe Snowflake
  , msgPreviousEditId  :: Maybe Snowflake
  , msgTime            :: UTCTime
  , msgSender          :: SessionView
  , msgContent         :: T.Text
  , msgEncryptionKeyId :: Maybe T.Text
  , msgEdited          :: Maybe UTCTime
  , msgDeleted         :: Maybe UTCTime
  , msgTruncated       :: Bool
  } deriving (Show)

instance FromJSON Message where
  parseJSON v = parseJSON v >>= \o -> Message
    <$> o .: "id"
    <*> o .:? "parent"
    <*> o .:? "previous_edit_id"
    <*> (posixSecondsToUTCTime <$> o .: "time")
    <*> o .: "sender"
    <*> o .: "content"
    <*> o .:? "encryption_key_id"
    <*> o .:? "edited"
    <*> (fmap posixSecondsToUTCTime <$> o .:? "deleted")
    <*> o .:? "truncated" .!= False

data PersonalAccountView = PersonalAccountView
  { pavId    :: Snowflake
  , pavName  :: T.Text
  , pavEmail :: T.Text
  } deriving (Show)

instance FromJSON PersonalAccountView where
  parseJSON v = parseJSON v >>= \o -> PersonalAccountView
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "email"

-- | A 'SessionView' describes a session and its identity. See
-- <http://api.euphoria.io/#sessionview>.
data SessionView = SessionView
  { svId                :: UserId
  , svNick              :: T.Text
  , svServerId          :: T.Text
  , svServerEra         :: T.Text
  , svSessionId         :: T.Text
  , svIsStaff           :: Bool
  , svIsManager         :: Bool
  , svClientAddress     :: Maybe T.Text
  , svRealClientAddress :: Maybe T.Text
  } deriving (Show)

instance FromJSON SessionView where
  parseJSON v = parseJSON v >>= \o -> SessionView
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "server_id"
    <*> o .: "server_era"
    <*> o .: "session_id"
    <*> o .:? "is_staff" .!= False
    <*> o .:? "is_manager" .!= False
    <*> o .:? "client_address"
    <*> o .:? "real_client_address"

-- | A snowflake is a 13-character string, usually used as a unique identifier
-- for some type of object. It is the base-36 encoding of an unsigned, 64-bit
-- integer. See <http://api.euphoria.io/#snowflake>.
type Snowflake = T.Text

-- | The type of session a client may have.
data UserType
  = Agent
  -- ^ The client is a person that is not logged in to any account.
  | Account
  -- ^ The client is a person that is logged into an account.
  | Bot
  -- ^ The client is a bot. Bots can never be logged in.
  | Other
  -- ^ The client has none of the other user types. While this value does not
  -- occur nowadays, some messages in the room logs are still from a time before
  -- the distinction of user types were introduced.
  deriving (Show, Eq)

-- | A 'UserId' identifies a user. It consists of two parts: The type of
-- session, and a unique value for that type of session. See
-- <http://api.euphoria.io/#userid>.
data UserId = UserId
  { userType      :: UserType
  , userSnowflake :: Snowflake
  } deriving (Show, Eq)

instance ToJSON UserId where
  toJSON uid =
    let prefix = case userType uid of
          Agent   -> "agent:"
          Account -> "account:"
          Bot     -> "bot:"
          Other   -> ""
    in  String $ prefix <> userSnowflake uid

instance FromJSON UserId where
  parseJSON v = parseJSON v >>= \s -> case T.breakOn ":" s of
    (snowflake, "")        -> pure $ UserId Other snowflake
    ("agent", snowflake)   -> pure $ UserId Agent   $ T.drop 1 snowflake
    ("account", snowflake) -> pure $ UserId Account $ T.drop 1 snowflake
    ("bot", snowflake)     -> pure $ UserId Bot     $ T.drop 1 snowflake
    _                      -> fail "invalid user id label"

{- Asynchronous events -}

{- bounce-event -}

data BounceEvent = BounceEvent
  { bounceReason     :: Maybe T.Text
  , bounceAuthOption :: [AuthOption]
  } deriving (Show)

instance FromJSON BounceEvent where
  parseJSON = fromPacket "bounce-event" $ \o -> BounceEvent
    <$> o .:? "reason"
    <*> o .:? "auth_options" .!= []

{- disconnect-event -}

newtype DisconnectEvent = DisconnectEvent
  { disconnectReason :: T.Text
  } deriving (Show)

instance FromJSON DisconnectEvent where
  parseJSON = fromPacket "disconnect-evnet" $ \o -> DisconnectEvent
    <$> o .: "reason"

{- hello-event -}

data HelloEvent = HelloEvent
  { helloAccount              :: Maybe PersonalAccountView
  , helloSessionView          :: SessionView
  , helloAccountHasAccess     :: Maybe Bool
  , helloAccountEmailVerified :: Maybe Bool
  , helloRoomIsPrivate        :: Bool
  , helloVersion              :: T.Text
  } deriving (Show)

instance FromJSON HelloEvent where
  parseJSON = fromPacket "hello-event" $ \o -> HelloEvent
    <$> o .:? "account"
    <*> o .: "session"
    <*> o .:? "account_has_access"
    <*> o .:? "account_email_verified"
    <*> o .: "room_is_private"
    <*> o .: "version"

{- join-event -}

newtype JoinEvent = JoinEvent
  { joinSession :: SessionView
  } deriving (Show)

instance FromJSON JoinEvent where
  parseJSON = fromPacket "join-event" $ \o -> JoinEvent
    <$> parseJSON (Object o)

{- login-event -}

newtype LoginEvent = LoginEvent
  { loginAccountId :: Snowflake
  } deriving (Show)

instance FromJSON LoginEvent where
  parseJSON = fromPacket "login-event" $ \o -> LoginEvent
    <$> o .: "acount_id"

{- logout-event -}

data LogoutEvent = LogoutEvent
  deriving (Show)

instance FromJSON LogoutEvent where
  parseJSON = fromPacket "logout-event" $ const (pure LogoutEvent)

{- network-event -}

data NetworkEvent = NetworkEvent
  { networkType      :: T.Text -- always "partition"
  , networkServerId  :: T.Text
  , networkServerEra :: T.Text
  } deriving (Show)

instance FromJSON NetworkEvent where
  parseJSON = fromPacket "network-event" $ \o -> NetworkEvent
    <$> o .: "type"
    <*> o .: "server_id"
    <*> o .: "server_era"

{- nick-event -}

data NickEvent = NickEvent
  { nickSessionId :: T.Text
  , nickId        :: UserId
  , nickFrom      :: T.Text
  , nickTo        :: T.Text
  } deriving (Show)

instance FromJSON NickEvent where
  parseJSON = fromPacket "nick-event" $ \o -> NickEvent
    <$> o .: "session_id"
    <*> o .: "id"
    <*> o .: "from"
    <*> o .: "to"

{- edit-message-event -}

data EditMessageEvent = EditMessageEvent
  { editMessageMessage :: Message
  , editMessageEditId  :: Snowflake
  } deriving (Show)

instance FromJSON EditMessageEvent where
  parseJSON = fromPacket "EditMessageEvent" $ \o -> EditMessageEvent
    <$> parseJSON (Object o)
    <*> o .: "edit_id"

{- part-event -}

newtype PartEvent = PartEvent
  { partSession :: SessionView
  } deriving (Show)

instance FromJSON PartEvent where
  parseJSON = fromPacket "part-event" $ \o -> PartEvent
    <$> parseJSON (Object o)

{- ping-event -}

data PingEvent = PingEvent
  { pingTime :: UTCTime
  , pingNext :: UTCTime
  } deriving (Show)

instance FromJSON PingEvent where
  parseJSON = fromPacket "ping-event" $ \o -> PingEvent
    <$> (posixSecondsToUTCTime <$> o .: "time")
    <*> (posixSecondsToUTCTime <$> o .: "next")

{- pm-initiate-event -}

data PmInitiateEvent = PmInitiateEvent
  { pmInitiateFrom     :: UserId
  , pmInitiateFromNick :: T.Text
  , pmInitiateFromRoom :: T.Text
  , pmInitiatePmId     :: Snowflake
  } deriving (Show)

instance FromJSON PmInitiateEvent where
  parseJSON = fromPacket "pm-initiate-event" $ \o -> PmInitiateEvent
    <$> o .: "from"
    <*> o .: "from_nick"
    <*> o .: "from_room"
    <*> o .: "pm_id"

{- send-event -}

newtype SendEvent = SendEvent
  { sendMessage :: Message
  } deriving (Show)

instance FromJSON SendEvent where
  parseJSON = fromPacket "send-event" $ \o -> SendEvent
    <$> parseJSON (Object o)

{- snapshot-event -}

data SnapshotEvent = SnapshotEvent
  { snapshotIdentity     :: UserId
  , snapshotSessionId    :: T.Text
  , snapshotVersion      :: T.Text
  , snapshotListing      :: [SessionView]
  , snapshotLog          :: [Message]
  , snapshotNick         :: Maybe T.Text
  , snapshotPmWithNick   :: Maybe T.Text
  , snapshotPmWithUserId :: Maybe UserId
  } deriving (Show)

instance FromJSON SnapshotEvent where
  parseJSON = fromPacket "snapshot-event" $ \o -> SnapshotEvent
    <$> o .: "identity"
    <*> o .: "session_id"
    <*> o .: "version"
    <*> o .: "listing"
    <*> o .: "log"
    <*> o .:? "nick"
    <*> o .:? "pm_with_nick"
    <*> o .:? "pm_with_user_id"

{- Session commands -}

{- auth -}

newtype AuthCommand = AuthWithPasscode T.Text
  deriving (Show)

instance ToJSONObject AuthCommand where
  toJSONObject (AuthWithPasscode password) = toPacket "auth" $ object
    [ "type" .= Passcode
    , "passcode" .= password
    ]

data AuthReply = AuthSuccessful | AuthFailed T.Text
  deriving (Show)

instance FromJSON AuthReply where
  parseJSON = fromPacket "auth-reply" $ \o -> do
    success <- o .: "success"
    if success
      then pure AuthSuccessful
      -- The "reason" field *should* be filled in if the authentication fails,
      -- but I'm always treating it as optional to be on the safe side.
      else AuthFailed <$> o .:? "reason" .!= ""

{- ping -}

newtype PingCommand = PingCommand UTCTime
  deriving (Show)

instance ToJSONObject PingCommand where
  toJSONObject (PingCommand time) = toPacket "ping-reply" $ object
    [ "time" .= utcTimeToPOSIXSeconds time
    ]

newtype PingReply = PingReply UTCTime
  deriving (Show)

instance ToJSONObject PingReply where
  toJSONObject (PingReply time) = toPacket "ping-reply" $ object
    [ "time" .= utcTimeToPOSIXSeconds time
    ]

instance FromJSON PingReply where
  parseJSON = fromPacket "ping-reply" $ \o -> PingReply
    <$> (posixSecondsToUTCTime <$> o .: "time")

{- Chat room commands -}

{- get-message -}

newtype GetMessageCommand = GetMessageCommand Snowflake
  deriving (Show)

instance ToJSONObject GetMessageCommand where
  toJSONObject (GetMessageCommand mId) = toPacket "get-message" $ object
    [ "id" .= mId
    ]

newtype GetMessageReply = GetMessageReply Message
  deriving (Show)

instance FromJSON GetMessageReply where
  parseJSON = fromPacket "get-message-reply" $ \o -> GetMessageReply
    <$> parseJSON (Object o)

{- log -}

data LogCommand = LogCommand Int (Maybe Snowflake)
  deriving (Show)

instance ToJSONObject LogCommand where
  toJSONObject (LogCommand n Nothing) = toPacket "log" $ object
    [ "n" .= n
    ]
  toJSONObject (LogCommand n (Just before)) = toPacket "log" $ object
    [ "n" .= n
    , "before" .= before
    ]

data LogReply = LogReply [Message] (Maybe Snowflake)
  deriving (Show)

instance FromJSON LogReply where
  parseJSON = fromPacket "log-reply" $ \o -> LogReply
    <$> o .: "log"
    <*> o .:? "before"

{- nick -}

newtype NickCommand = NickCommand T.Text
  deriving (Show)

instance ToJSONObject NickCommand where
  toJSONObject (NickCommand nick) = toPacket "nick" $ object
    [ "name" .= nick
    ]

data NickReply = NickReply
  { nickReplySessionId :: T.Text
  , nickReplyId        :: UserId
  , nickReplyFrom      :: T.Text
  , nickReplyTo        :: T.Text
  } deriving (Show)

instance FromJSON NickReply where
  parseJSON = fromPacket "nick-reply" $ \o -> NickReply
    <$> o .: "session_id"
    <*> o .: "id"
    <*> o .: "from"
    <*> o .: "to"

{- pm-initiate -}

newtype PmInitiateCommand = PmInitiateCommand UserId
  deriving (Show)

instance ToJSONObject PmInitiateCommand where
  toJSONObject (PmInitiateCommand userId) = toPacket "pm-initiate" $ object
    [ "user_id" .= userId
    ]

data PmInitiateReply = PmInitiateReply Snowflake T.Text
  deriving (Show)

instance FromJSON PmInitiateReply where
  parseJSON = fromPacket "pm-intiate-reply" $ \o -> PmInitiateReply
    <$> o .: "pm_id"
    <*> o .: "to_nick"

{- send -}

data SendCommand = SendCommand T.Text (Maybe Snowflake)
  deriving (Show)

instance ToJSONObject SendCommand where
  toJSONObject (SendCommand content Nothing) =
    toPacket "send" $ object ["content" .= content]
  toJSONObject (SendCommand content (Just parent)) =
    toPacket "send" $ object ["content" .= content, "parent" .= parent]

newtype SendReply = SendReply Message
  deriving (Show)

instance FromJSON SendReply where
  parseJSON = fromPacket "send-reply" $ \o -> SendReply
    <$> parseJSON (Object o)

{- who -}

data WhoCommand = WhoCommand
  deriving (Show)

instance ToJSONObject WhoCommand where
  toJSONObject WhoCommand = toPacket "who" $ object []

newtype WhoReply = WhoReply [SessionView]
  deriving (Show)

instance FromJSON WhoReply where
  parseJSON = fromPacket "who-reply" $ \o -> WhoReply
    <$> o .: "listing"

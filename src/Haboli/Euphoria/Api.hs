{-# LANGUAGE OverloadedStrings #-}

module Haboli.Euphoria.Api
  ( ToJSONObject(..)
  -- * Basic types
  , AuthOption(..)
  , Message(..)
  , PersonalAccountView
  , SessionView(..)
  , Snowflake
  , UserType(..)
  , UserId(..)
  -- * Asynchronous events
  , BounceEvent(..)
  , DisconnectEvent(..)
  , HelloEvent(..)
  , JoinEvent(..)
  , LoginEvent(..)
  , LogoutEvent(..)
  , NetworkEvent(..)
  , NickEvent(..)
  , EditMessageEvent(..)
  , PartEvent(..)
  , PingEvent(..)
  , PmInitiateEvent(..)
  , SendEvent(..)
  , SnapshotEvent(..)
  -- * Session commands
  -- ** auth
  , AuthCommand(..)
  , AuthReply(..)
  -- ** ping
  , PingCommand(..)
  , PingReply(..)
  -- * Chat room commands
  -- ** nick
  , NickCommand(..)
  , NickReply(..)
  ) where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict   as HMap
import qualified Data.Text             as T
import           Data.Time
import           Data.Time.Clock.POSIX

class ToJSONObject a where
  toJSONObject :: a -> Object

fromPacket :: T.Text -> (Object -> Parser a) -> Value -> Parser a
fromPacket packetType parser (Object o) = do
  actualType <- o .: "type"
  when (actualType /= packetType) $
    fail $ T.unpack $ "packet type is not " <> packetType
  packetData <- o .: "data"
  parser packetData
fromPacket _ _ v = typeMismatch "Object" v

toPacket :: T.Text -> Value -> Object
toPacket packetType packetData = HMap.fromList
  [ "type" .= packetType
  , "data" .= packetData
  ]

{- Basic types -}

data AuthOption = Passcode
  deriving (Show)

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
  { msgId        :: Snowflake
  , msgParent    :: Maybe Snowflake
  -- , msgPreviousEditId  :: Maybe Snowflake
  , msgTime      :: UTCTime
  , msgSender    :: SessionView
  , msgContent   :: T.Text
  -- , msgEncryptionKeyId :: Maybe T.Text
  -- , msgEdited          :: Maybe UTCTime
  , msgDeleted   :: Maybe UTCTime
  , msgTruncated :: Bool
  } deriving (Show)

instance FromJSON Message where
  parseJSON (Object v) = Message
    <$> v .: "id"
    <*> v .:? "parent"
    -- <*> v .:? "previous_edit_id"
    <*> (posixSecondsToUTCTime <$> v .: "time")
    <*> v .: "sender"
    <*> v .: "content"
    -- <*> v .:? "encryption_key_id"
    -- <*> v .:? "edited"
    <*> (fmap posixSecondsToUTCTime <$> v .:? "deleted")
    <*> v .:? "truncated" .!= False
  parseJSON v = typeMismatch "Object" v

data PersonalAccountView = PersonalAccountView
  { pavId    :: Snowflake
  , pavName  :: T.Text
  , pavEmail :: T.Text
  } deriving (Show)

-- | A 'SessionView' describes a session and its identity. See
-- <http://api.euphoria.io/#sessionview>.
data SessionView = SessionView
  { svId        :: UserId
  , svNick      :: T.Text
  , svServerId  :: T.Text
  , svServerEra :: T.Text
  , svSessionId :: T.Text
  , svIsStaff   :: Bool
  , svIsManager :: Bool
  -- , svClientAddress     :: Maybe T.Text
  -- , svRealClientAddress :: Maybe T.Text
  } deriving (Show)

instance FromJSON SessionView where
  parseJSON (Object v) = SessionView
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "server_id"
    <*> v .: "server_era"
    <*> v .: "session_id"
    <*> v .:? "is_staff" .!= False
    <*> v .:? "is_manager" .!= False
    -- <*> v .:? "client_address"
    -- <*> v .:? "real_client_address"
  parseJSON v = typeMismatch "Object" v

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

instance FromJSON UserId where
  parseJSON (String v) = case T.breakOn ":" v of
    (snowflake, "")        -> pure $ UserId Other snowflake
    ("agent", snowflake)   -> pure $ UserId Agent   $ T.drop 1 snowflake
    ("account", snowflake) -> pure $ UserId Account $ T.drop 1 snowflake
    ("bot", snowflake)     -> pure $ UserId Bot     $ T.drop 1 snowflake
    _                      -> fail "invalid user id label"
  parseJSON v = typeMismatch "String" v

{- Asynchronous events -}

data BounceEvent = BounceEvent
  { bounceReason     :: Maybe T.Text
  , bounceAuthOption :: [AuthOption]
  } deriving (Show)

data DisconnectEvent = DisconnectEvent
  { disconnectReason :: T.Text
  } deriving (Show)

--TODO: Merge the account stuff with the PersonalAccountView?
data HelloEvent = HelloEvent
  { helloAccount              :: Maybe PersonalAccountView
  , helloSessionView          :: SessionView
  , helloAccountHasAccess     :: Maybe Bool
  , helloAccountEmailVerified :: Maybe Bool
  , helloRoomIsPrivate        :: Bool
  , helloVersion              :: T.Text
  } deriving (Show)

data JoinEvent = JoinEvent
  { joinSession :: SessionView
  } deriving (Show)

data LoginEvent = LoginEvent
  { loginAccountId :: Snowflake
  } deriving (Show)

data LogoutEvent = LogoutEvent
  deriving (Show)

data NetworkEvent = NetworkEvent
  { networkType      :: T.Text -- always "partition"
  , networkServerId  :: T.Text
  , networkServerEra :: T.Text
  } deriving (Show)

data NickEvent = NickEvent
  { nickSessionId :: T.Text
  , nickId        :: UserId
  , nickFrom      :: T.Text
  , nickTo        :: T.Text
  } deriving (Show)

data EditMessageEvent = EditMessageEvent
  { editMessageMessage :: Message
  , editMessageEditId  :: Snowflake
  } deriving (Show)

data PartEvent = PartEvent
  { partSession :: SessionView
  } deriving (Show)

data PingEvent = PingEvent
  { pingTime :: UTCTime
  , pingNext :: UTCTime
  } deriving (Show)

instance FromJSON PingEvent where
  parseJSON = fromPacket "ping-event" $ \o -> PingEvent
    <$> (posixSecondsToUTCTime <$> o .: "time")
    <*> (posixSecondsToUTCTime <$> o .: "next")

data PmInitiateEvent = PmInitiateEvent
  { pmInitiateFrom     :: UserId
  , pmInitiateFromNick :: T.Text
  , pmInitiateFromRoom :: T.Text
  , pmInitiatePmId     :: Snowflake
  } deriving (Show)

data SendEvent = SendEvent
  { sendMessage :: Message
  } deriving (Show)

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

data AuthCommand = AuthWithPasscode T.Text
  deriving (Show)

data AuthReply = AuthSuccessful | AuthFailed T.Text
  deriving (Show)

{- ping -}

data PingCommand = PingCommand UTCTime
  deriving (Show)

instance ToJSONObject PingCommand where
  toJSONObject (PingCommand time) = toPacket "ping-reply" $ object
    [ "time" .= utcTimeToPOSIXSeconds time
    ]

data PingReply = PingReply UTCTime
  deriving (Show)

instance ToJSONObject PingReply where
  toJSONObject (PingReply time) = toPacket "ping-reply" $ object
    [ "time" .= utcTimeToPOSIXSeconds time
    ]

instance FromJSON PingReply where
  parseJSON = fromPacket "ping-reply" $ \o -> PingReply
    <$> (posixSecondsToUTCTime <$> o .: "time")

{- Chat room commands -}

{- nick -}

data NickCommand = NickCommand T.Text
  deriving (Show)

instance ToJSONObject NickCommand where
  toJSONObject (NickCommand nick) = HMap.fromList
    [ "type" .= String "nick"
    , "data" .= object ["name" .= nick]
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

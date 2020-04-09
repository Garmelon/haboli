{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | This module contains an example implementation of a small bot. It is a good
-- starting point if you want to create your own bot.
--
-- The example bot uses lenses for its state because they vastly reduce the
-- amount of code required to update the 'Listing' inside the state. It is
-- entirely possible to use haboli without lenses though, should you want to do
-- that.

module Haboli.Euphoria.ExampleBot
  ( exampleBot
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as T
import           Data.Time
import           Lens.Micro
import           Lens.Micro.TH

import           Haboli.Euphoria
import           Haboli.Euphoria.Botrulez

data BotState = BotState
  { _botStartTime :: UTCTime
  , _botListing   :: Listing
  } deriving (Show)

makeLenses ''BotState

-- | A small example bot. Takes a room password as its first argument. You can
-- run this bot in [&test](https://euphoria.io/room/test) like this:
--
-- > runClient defaultConfig $ exampleBot Nothing
exampleBot :: Maybe T.Text -> Client T.Text ()
exampleBot mPasswd = do
  startTime <- liftIO getCurrentTime
  initialEvents <- untilConnected $
    respondingToBounce mPasswd $
    respondingToPing nextEvent
  let initialState = BotState startTime $ newListing initialEvents
  stateVar <- liftIO $ newMVar initialState
  preferNickVia botListing stateVar "ExampleBot"
  botMain stateVar

botMain :: MVar BotState -> Client T.Text ()
botMain stateVar = forever $ do
  event <- respondingToCommands (getCommands stateVar) $
    respondingToPing nextEvent
  updateFromEventVia botListing stateVar event

getCommands :: MVar BotState -> Client e [Command T.Text]
getCommands stateVar = do
  state <- liftIO $ readMVar stateVar
  let name = state ^. botListing . lsSelfL . svNickL
  pure
    [ botrulezPingGeneral
    , botrulezPingSpecific name
    , botrulezHelpSpecific name
        "I am an example bot for https://github.com/Garmelon/haboli/."
    , botrulezUptimeSpecific name $ state ^. botStartTime
    , botrulezKillSpecific name
    , cmdSpecific "hug" name $ \msg -> void $ reply msg "/me hugs back"
    , cmdHello
    , cmdNick stateVar name
    , cmdWho stateVar
    ]

cmdHello :: Command e
cmdHello = cmdGeneral "hello" $ \msg -> do
  let mention = nickMention $ svNick $ msgSender msg
  void $ reply msg $ "Hi there, @" <> mention <> "!"

cmdNick :: MVar BotState -> T.Text -> Command e
cmdNick stateVar name = cmdSpecificArgs "nick" name $ \msg args -> do
  preferNickVia botListing stateVar args
  void $ reply msg "Is this better?"

cmdWho :: MVar BotState -> Command e
cmdWho stateVar = cmdGeneral "who" $ \msg -> do
  state <- liftIO $ readMVar stateVar
  let people = state ^. botListing . lsOthersL
      nicks = sort $ map svNick $ Map.elems people
  void $ reply msg $ T.intercalate "\n" nicks

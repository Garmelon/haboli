{-# LANGUAGE OverloadedStrings #-}

-- | This module contains an example implementation of a small bot. It is a good
-- starting point if you want to create your own bot.

module Haboli.Euphoria.ExampleBot
  ( exampleBot
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.Text                 as T
import           Data.Time

import           Haboli.Euphoria
import           Haboli.Euphoria.Botrulez

data BotState = BotState
  { botStartTime :: UTCTime
  , botListing   :: Listing
  } deriving (Show)

type Bot = StateT BotState (Client T.Text)

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
  listing <- preferNick "ExampleBot" $ newListing initialEvents
  void $ runStateT botMain $ BotState startTime listing

botMain :: Bot ()
botMain = forever $ do
  s <- get
  let name = svNick $ self $ botListing s
  lift $ respondingToCommands
    [ botrulezPingGeneral
    , botrulezPingSpecific name
    , botrulezHelpSpecific name "I am an example bot for https://github.com/Garmelon/haboli/."
    , botrulezUptimeSpecific name $ botStartTime s
    , botrulezKillSpecific name
    , cmdGeneral "hello" $ \msg ->
        void $ reply msg $ "Hi there, " <> nickMention (svNick $ msgSender msg) <> "!"
    , cmdSpecific "hug" name $ \msg ->
        void $ reply msg "/me hugs back"
    ] $ respondingToPing nextEvent


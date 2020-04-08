{-# LANGUAGE OverloadedStrings #-}

-- | This module contains an example implementation of a small bot. It is a good
-- starting point if you want to create your own bot.

module Haboli.Euphoria.ExampleBot
  ( exampleBot
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Text                as T
import           Data.Time

import           Haboli.Euphoria
import           Haboli.Euphoria.Botrulez

data BotState = BotState
  { botStartTime :: UTCTime
  , botListing   :: Listing
  } deriving (Show)

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
  stateVar <- liftIO $ newMVar $ BotState startTime listing
  botMain stateVar

botMain :: MVar BotState -> Client T.Text ()
botMain stateVar = forever $ respondingToPing $ respondingToCommands nextEvent $ do
  state <- liftIO $ readMVar stateVar
  let name = svNick $ self $ botListing state
  pure
    [ botrulezPingGeneral
    , botrulezPingSpecific name
    , botrulezHelpSpecific name "I am an example bot for https://github.com/Garmelon/haboli/."
    , botrulezUptimeSpecific name $ botStartTime state
    , botrulezKillSpecific name
    , cmdSpecific "hug" name $ \msg -> void $ reply msg "/me hugs back"
    , cmdGeneral "hello" $ \msg ->
        void $ reply msg $ "Hi there, " <> nickMention (svNick $ msgSender msg) <> "!"
    , cmdSpecificArgs "nick" name $ \msg args -> do
        s <- liftIO $ takeMVar stateVar
        listing' <- preferNick args $ botListing s
        liftIO $ putMVar stateVar s{botListing = listing'}
        void $ reply msg "Is this better?"
    ]

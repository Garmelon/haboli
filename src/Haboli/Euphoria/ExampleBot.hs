{-# LANGUAGE OverloadedStrings #-}

-- | This module contains an example implementation of a small bot. It is a good
-- starting point if you want to create your own bot.

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
botMain stateVar = forever $ do
  event <- respondingToCommands (getCommands stateVar) $
    respondingToPing nextEvent
  -- Update the listing
  liftIO $ modifyMVar_ stateVar $ \state ->
    pure state{botListing = updateFromEvent event $ botListing state}

getCommands :: MVar BotState -> Client e [Command T.Text]
getCommands stateVar = do
  state <- liftIO $ readMVar stateVar
  let name = svNick $ self $ botListing state
  pure
    [ botrulezPingGeneral
    , botrulezPingSpecific name
    , botrulezHelpSpecific name "I am an example bot for https://github.com/Garmelon/haboli/."
    , botrulezUptimeSpecific name $ botStartTime state
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
  -- Update the listing while updating the nick
  state <- liftIO $ takeMVar stateVar
  listing' <- preferNick args $ botListing state
  liftIO $ putMVar stateVar state{botListing = listing'}
  void $ reply msg "Is this better?"

cmdWho :: MVar BotState -> Command e
cmdWho stateVar = cmdGeneral "who" $ \msg -> do
  state <- liftIO $ readMVar stateVar
  let people = others $ botListing state
      nicks = sort $ map svNick $ Map.elems people
  void $ reply msg $ T.intercalate "\n" nicks

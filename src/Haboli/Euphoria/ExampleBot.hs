{-# LANGUAGE OverloadedStrings #-}

-- | This module contains an example implementation of a small bot. It is a good
-- starting point if you want to create your own bot.

module Haboli.Euphoria.ExampleBot
  ( exampleBot
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.Text                 as T

import           Haboli.Euphoria

newtype BotState = BotState
  { botListing :: Listing
  } deriving (Show)

type Bot = StateT BotState (Client T.Text)

-- | A small example bot. Takes a room password as its first argument. You can
-- run this bot in [&test](https://euphoria.io/room/test) like this:
--
-- > runClient defaultConfig $ exampleBot Nothing
exampleBot :: Maybe T.Text -> Client T.Text ()
exampleBot mPasswd = do
  initialEvents <- untilConnected $
    respondingToBounce mPasswd $
    respondingToPing nextEvent
  listing <- preferNick "ExampleBot" $ newListing initialEvents
  void $ runStateT botMain $ BotState listing

botMain :: Bot ()
botMain = forever $ do
  s <- get
  let name = svNick $ self $ botListing s
  lift $ respondingToCommands
    [ cmdGeneral "ping" $ \msg -> void $ reply msg "Pong!"
    , cmdSpecific "ping" name $ \msg -> void $ reply msg "Pong!"
    , cmdSpecific "help" name $ \msg ->
        void $ reply msg "I am an example bot for https://github.com/Garmelon/haboli/."
    , cmdSpecific "kill" name $ \msg -> do
        void $ reply msg "/me dies"
        throw $ "I was killed by " <> svNick (msgSender msg)
    ] $ respondingToPing nextEvent


{-# LANGUAGE OverloadedStrings #-}

module Haboli.Euphoria.ExampleBot
  ( BotState(..)
  , exampleBot
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.Text                      as T

import           Haboli.Euphoria
import           Haboli.Euphoria.Command
import           Haboli.Euphoria.Command.Simple
import           Haboli.Euphoria.Listing
import           Haboli.Euphoria.Util

newtype BotState = BotState
  { botListing :: Listing
  } deriving (Show)

type Bot = StateT BotState (Client T.Text)

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


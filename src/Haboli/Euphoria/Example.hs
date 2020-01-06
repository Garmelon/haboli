{-# LANGUAGE OverloadedStrings #-}

-- | This module contains a few basic example bots.
module Haboli.Euphoria.Example where

import           Control.Monad
import           Control.Monad.IO.Class
import           Haboli.Euphoria.Client

printAllEventsBot :: Client () ()
printAllEventsBot = forever $ do
  liftIO $ putStrLn "Waiting for the next event"
  liftIO . print =<< respondingToPing nextEvent

setNickAndThenWaitBot :: Client () ()
setNickAndThenWaitBot = forever $ do
  event <- respondingToPing nextEvent
  case event of
    EventSnapshot _ -> void $ nick "HaboliTestBot"
    _               -> pure ()

throwCustomExceptionBot :: Client String ()
throwCustomExceptionBot = throw "Hello world"

immediatelyDisconnectBot :: Client () ()
immediatelyDisconnectBot = pure ()

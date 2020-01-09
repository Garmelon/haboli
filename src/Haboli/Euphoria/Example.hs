{-# LANGUAGE OverloadedStrings #-}

-- | This module contains a few basic example bots.
module Haboli.Euphoria.Example where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           Haboli.Euphoria.Api
import           Haboli.Euphoria.Client

printAllEventsBot :: Client () ()
printAllEventsBot = forever $ do
  liftIO $ putStrLn "\nWaiting for the next event...\n"
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

sendMessagesUntilThrottledBot :: Client () ()
sendMessagesUntilThrottledBot = forever $ do
  event <- respondingToPing nextEvent
  case event of
    EventSnapshot _ -> do
      void $ nick "SpamBot"
      msg <- send "start thread"
      void $ fork $ handle (\_ -> reply msg "got throttled") $
        forever $ reply msg "continue thread"
    _ -> pure ()

sendMessagesThreadedBot :: Client () ()
sendMessagesThreadedBot = forever $ do
  event <- respondingToPing nextEvent
  case event of
    EventSnapshot _ -> void $ nick "TreeBot"
    EventSend e ->
      let msg = sendMessage e
      in  when (msgContent msg == "!tree") $
            void $ fork $ buildTree msg
    _ -> pure ()
  where
    buildTree msg = do
      t1 <- fork $ reply msg "subtree 1"
      t2 <- fork $ reply msg "subtree 2"
      subtree1 <- wait t1
      subtree2 <- wait t2
      t3 <- fork $ reply subtree1 "subtree 1.1"
      t4 <- fork $ reply subtree1 "subtree 1.2"
      t5 <- fork $ reply subtree2 "subtree 2.1"
      t6 <- fork $ reply subtree2 "subtree 2.2"
      for_ [t3, t4, t5, t6] wait
      reply msg "tree done"

cloneItselfBot :: Client () ()
cloneItselfBot = forever $ do
  event <- respondingToPing nextEvent
  case event of
    EventSnapshot _ -> void $ nick "CloneBot"
    EventSend e
      | msgContent (sendMessage e) == "!clone" -> do
        config <- getConnectionConfig
        void $ liftIO $ forkIO $ void $ runClient config cloneItselfBot
      | otherwise -> pure ()
    _ -> pure ()

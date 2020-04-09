-- | This module provides an abstraction for bot commands.

module Haboli.Euphoria.Command
  ( Command
  , cmdSequential
  , cmdParallel
  , respondingToCommand
  ) where

import           Control.Monad

import           Haboli.Euphoria.Api
import           Haboli.Euphoria.Client

-- | If a command should block any further commands from executing on a message,
-- it should return 'True'. Otherwise. it should return 'False'.
type Command e = Message -> Client e Bool

-- | Try out multiple 'Command's in order until one returns 'True'. All commands
-- following that one are not applied. Returns 'True' if any of the commands
-- returned 'True'. Returns 'False' otherwise.
cmdSequential :: [Command e] -> Command e
cmdSequential [] _ = pure False
cmdSequential (c:cs) msg = do
  abort <- c msg
  if abort
    then pure True
    else cmdSequential cs msg

-- | Apply multiple 'Command's in order. Each command will be applied. Returns
-- 'True' if at least one of the commands returned 'True'. Returns 'False'
-- otherwise.
cmdParallel :: [Command e] -> Command e
cmdParallel commands msg = do
  results <- traverse ($msg) commands
  pure $ or results

-- | @'respondingToCommand' getCommand getEvent@ runs a 'Command' on all
-- 'EventSend's. It passes through all events unmodified.
--
-- The @getEvent@ action is used to obtain the next 'Event'. The @getCommand@
-- action is used to obtain the currently available command. @getCommand@ is
-- called directly after a new 'Event' becomes available through @getEvent@.
--
-- This utility function is meant to be wrapped directly or indirectly around
-- 'nextEvent':
--
-- > event <- respondingToCommand command nextEvent
respondingToCommand :: Client e (Command e) -> Client e Event -> Client e Event
respondingToCommand getCommand getEvent = do
  event <- getEvent
  command <- getCommand
  case event of
    EventSend e -> void $ command $ sendMessage e
    _           -> pure ()
  pure event

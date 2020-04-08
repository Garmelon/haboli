-- | This module provides an abstraction for bot commands.

module Haboli.Euphoria.Command
  ( Command
  , runCommands
  , respondingToCommands
  ) where

import           Control.Monad

import           Haboli.Euphoria.Api
import           Haboli.Euphoria.Client

-- | If a command should block any further commands from executing on a message,
-- it should return 'True'. Otherwise. it should return 'False'.
type Command e = Message -> Client e Bool

-- | Apply multiple 'Command's to a 'Message' in order until one returns 'True'.
-- All commands following that one are not applied.
runCommands :: [Command e] -> Message -> Client e Bool
runCommands [] _ = pure False
runCommands (c:cs) msg = do
  abort <- c msg
  if abort
    then pure True
    else runCommands cs msg

-- | @'respondingToCommands' getEvent getCommands@ runs a list of 'Command's on
-- all 'EventSend's. It passes through all events unmodified.
--
-- The @getEvent@ action is used to obtain the next 'Event'. The @getCommands@
-- action is used to obtain the currently available commands. @getCommands@ is
-- called directly after a new 'Event' becomes available through @getEvent@.
--
-- This utility function is meant to be wrapped directly or indirectly around
-- 'nextEvent':
--
-- > event <- respondingToCommands nextEvent commands
respondingToCommands :: Client e Event -> Client e [Command e] -> Client e Event
respondingToCommands getEvent getCommands = do
  event <- getEvent
  commands <- getCommands
  case event of
    EventSend e -> void $ runCommands commands $ sendMessage e
    _           -> pure ()
  pure event

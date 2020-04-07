{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

-- | This module provides an abstraction for bot commands in the form of the
-- 'Hook' type class and the 'Command' type.

module Haboli.Euphoria.Command
  ( Hook(..)
  , Command
  , cmd
  , runCommand
  , runCommands
  , respondingToCommands
  ) where

import           Control.Monad

import           Haboli.Euphoria.Api
import           Haboli.Euphoria.Client

-- | A hook is a way to react to new messages in a room. These typically don't
-- include the messages sent by the client itself.
class Hook h where
  -- | @reel h msg@ applies the hook @h@ to the 'Message' @msg@. If no further
  -- hooks should be applied to this message, it should return 'True'.
  -- Otherwise, it should return 'False'.
  reel :: h e -> Message -> Client e Bool

-- | A wrapper around hooks that allows for heterogenous lists of hooks. In
-- other words, it lets you combine different 'Hook' instances into a single
-- list.
data Command e = forall h. (Hook h) => Command (h e)

-- | Wrap a hook. Notice how the @h@ type disappears: This function can convert
-- different 'Hook' instances into the same type.
cmd :: (Hook h) => h e -> Command e
cmd = Command

-- | Apply a 'Command' to a 'Message'. For more information, see 'reel'.
runCommand :: Command e -> Message -> Client e Bool
runCommand (Command h) = reel h

-- | Apply multiple 'Command's to a 'Message' in order until one returns 'True'.
-- All commands following that one are not applied.
runCommands :: [Command e] -> Message -> Client e Bool
runCommands [] _ = pure False
runCommands (c:cs) msg = do
  abort <- runCommand c msg
  if abort
    then pure True
    else runCommands cs msg

-- | Run a list of 'Command's on all 'EventSend's. Passes through all events
-- unmodified.
--
-- This utility function is meant to be wrapped directly or indirectly around
-- 'nextEvent':
--
-- > event <- respondingToCommands commands nextEvent
respondingToCommands :: [Command e] -> Client e Event -> Client e Event
respondingToCommands cmds holdingEvent = do
  event <- holdingEvent
  case event of
    EventSend e -> void $ runCommands cmds $ sendMessage e
    _           -> pure ()
  pure event

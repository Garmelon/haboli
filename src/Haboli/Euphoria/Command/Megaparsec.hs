-- | Bot commands based on the megaparsec library.

module Haboli.Euphoria.Command.Megaparsec
  ( cmdMega
  , cmdMega'
  ) where

import qualified Data.Text               as T
import           Text.Megaparsec

import           Haboli.Euphoria.Api
import           Haboli.Euphoria.Client
import           Haboli.Euphoria.Command

-- | Turn a megaparsec parser into a bot command. Applies the parser to the
-- content of the message. If the parser fails to parse the message content, the
-- command fails.
cmdMega :: Parsec e' T.Text a -> (Message -> a -> Client e ()) -> Command e
cmdMega parser f = cmdMega' parser $ \msg a -> True <$ f msg a

-- | A version of 'cmdMega' that allows the command function to decide whether
-- the command was successful or not.
cmdMega' :: Parsec e' T.Text a -> (Message -> a -> Client e Bool) -> Command e
cmdMega' parser f msg = case parse parser "" $ msgContent msg of
  Left _  -> pure False
  Right a -> f msg a

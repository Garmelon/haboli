module Haboli.Euphoria.Command.Megaparsec
  ( cmdMega
  ) where

import qualified Data.Text               as T
import           Text.Megaparsec

import           Haboli.Euphoria.Api
import           Haboli.Euphoria.Client
import           Haboli.Euphoria.Command

cmdMega :: Parsec e' T.Text a -> (Message -> a -> Client e ()) -> Command e
cmdMega parser f msg = case parse parser "" $ msgContent msg of
  Left _  -> pure False
  Right a -> True <$ f msg a

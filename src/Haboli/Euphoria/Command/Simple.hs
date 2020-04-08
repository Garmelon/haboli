{-# LANGUAGE OverloadedStrings #-}

-- | General and specific commands as described in the
-- [botrulez](https://github.com/jedevc/botrulez).

module Haboli.Euphoria.Command.Simple
  (
  -- * General commands
    cmdGeneral
  , cmdGeneral'
  , cmdGeneralArgs
  , cmdGeneralArgs'
  -- * Specific commands
  , cmdSpecific
  , cmdSpecific'
  , cmdSpecificArgs
  , cmdSpecificArgs'
  ) where

import           Control.Monad
import           Data.Char
import qualified Data.Text                          as T
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Haboli.Euphoria.Api
import           Haboli.Euphoria.Client
import           Haboli.Euphoria.Command
import           Haboli.Euphoria.Command.Megaparsec
import           Haboli.Euphoria.Util

type Parser = Parsec () T.Text

pCmd :: T.Text -> Parser ()
pCmd cmd = void $ label "command" $ char '!' *> string cmd

pNick :: T.Text -> Parser ()
pNick name = label "nick" $ do
  void $ char '@'
  name' <- takeWhile1P Nothing (not . isSpace)
  guard $ nickEqual name name'

pUntilEof :: Parser T.Text
pUntilEof = takeWhileP Nothing (const True)

pCmdGeneral :: T.Text -> Parser T.Text
pCmdGeneral cmd = pCmd cmd *> space *> pUntilEof

pCmdSpecific :: T.Text -> T.Text -> Parser T.Text
pCmdSpecific cmd name = pCmd cmd *> space1 *> pNick name *> space *> pUntilEof

-- | @'cmdGeneral' cmd f' is a general command with no arguments in the form of
-- @!cmd@.
cmdGeneral :: T.Text -> (Message -> Client e ()) -> Command e
cmdGeneral cmd f = cmdGeneral' cmd $ \msg -> True <$ f msg

-- | A version of 'cmdGeneral' that allows the command function to decide
-- whether the command was successful or not.
cmdGeneral' :: T.Text -> (Message -> Client e Bool) -> Command e
cmdGeneral' cmd f = cmdGeneralArgs' cmd $ \msg args -> if T.null args
  then f msg
  else pure False

-- | @'cmdGeneralArgs' cmd f' is a general command with arguments in the form of
-- @!cmd args@. @f@ is called with the source message and the arguments as
-- 'T.Text'.
cmdGeneralArgs :: T.Text -> (Message -> T.Text -> Client e ()) -> Command e
cmdGeneralArgs cmd f = cmdGeneralArgs' cmd $ \msg args -> True <$ f msg args

-- | A version of 'cmdGeneralArgs' that allows the command function to decide
-- whether the command was successful or not.
cmdGeneralArgs' :: T.Text -> (Message -> T.Text -> Client e Bool) -> Command e
cmdGeneralArgs' cmd = cmdMega' $ pCmdGeneral cmd

-- | @'cmdSpecific' cmd nick f@ is a specific command with no arguments in the
-- form of @!cmd \@nick@.
cmdSpecific :: T.Text -> T.Text -> (Message -> Client e ()) -> Command e
cmdSpecific cmd name f = cmdSpecific' cmd name $ \msg -> True <$ f msg

-- | A version of 'cmdSpecific' that allows the command function to decide
-- whether the command was successful or not.
cmdSpecific' :: T.Text -> T.Text -> (Message -> Client e Bool) -> Command e
cmdSpecific' cmd name f = cmdSpecificArgs' cmd name $ \msg args -> if T.null args
  then f msg
  else pure False

-- | @'cmdSpecificArgs' cmd nick f@ is a specific command with arguments in the
-- form of @!cmd \@nick args@. @f@ is called with the source message and the
-- arguments as 'T.Text'.
cmdSpecificArgs :: T.Text -> T.Text -> (Message -> T.Text -> Client e ()) -> Command e
cmdSpecificArgs cmd name f = cmdSpecificArgs' cmd name $ \msg args -> True <$ f msg args

-- | A version of 'cmdSpecificArgs' that allows the command function to decide
-- whether the command was successful or not.
cmdSpecificArgs' :: T.Text -> T.Text -> (Message -> T.Text -> Client e Bool) -> Command e
cmdSpecificArgs' cmd name = cmdMega' $ pCmdSpecific cmd name

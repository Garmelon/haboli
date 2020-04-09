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
  -- * Parsers for convenience
  , pAnyCmd
  , pCmd
  , pAnyNick
  , pNick
  , pUntilEof
  , pCmdGeneral
  , pCmdSpecific
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

-- | Parse any command of the form @!\<non-space character\>@.
pAnyCmd :: (Ord e) => Parsec e T.Text T.Text
pAnyCmd = label "command" $ char '!' *> takeWhileP Nothing (not . isSpace)

-- | @'pCmd' a@ parses commands of the form @!\<cmd\>@ where @cmd@ is equivalent
-- to @a@.
pCmd :: (Ord e) => T.Text -> Parsec e T.Text T.Text
pCmd cmd = do
  cmd' <- pAnyCmd
  guard $ cmd == cmd'
  pure cmd'

-- | Parse any nick of the form @\@\<non-space character\>@.
pAnyNick :: (Ord e) => Parsec e T.Text T.Text
pAnyNick = label "nick" $ do
  void $ char '@'
  takeWhile1P Nothing (not . isSpace)

-- | @'pNick' a@ parses nicks of the form @\@\<name\>@ where @name@ is
-- equivalent (but not necessarily equal) to @a@.
pNick :: (Ord e) => T.Text -> Parsec e T.Text T.Text
pNick name = do
  name' <- pAnyNick
  guard $ nickEqual name name'
  pure name'

-- | Consume the rest of the input. This parser should never fail.
pUntilEof :: (Ord e) => Parsec e T.Text T.Text
pUntilEof = takeWhileP Nothing (const True)

-- | @'pCmdGeneral' cmd@ parses a general command of the form @!\<cmd\>@.
pCmdGeneral :: (Ord e) => T.Text -> Parsec e T.Text T.Text
pCmdGeneral cmd = pCmd cmd *> space *> pUntilEof

-- | @'pCmdSpecific' cmd name@ parses a specific command of the form @!\<cmd\> \@\<name\>@.
pCmdSpecific :: (Ord e) => T.Text -> T.Text -> Parsec e T.Text T.Text
pCmdSpecific cmd name = pCmd cmd *> space1 *> pNick name *> space *> pUntilEof

-- | @'cmdGeneral' cmd f@ is a general command with no arguments in the form of
-- @!cmd@.
cmdGeneral :: T.Text -> (Message -> Client e ()) -> Command e
cmdGeneral cmd f = cmdGeneral' cmd $ \msg -> True <$ f msg

-- | A version of 'cmdGeneral' that allows the command function to decide
-- whether the command was successful or not.
cmdGeneral' :: T.Text -> (Message -> Client e Bool) -> Command e
cmdGeneral' cmd f = cmdGeneralArgs' cmd $ \msg args -> if T.null args
  then f msg
  else pure False

-- | @'cmdGeneralArgs' cmd f@ is a general command with arguments in the form of
-- @!cmd args@. @f@ is called with the source message and the arguments as
-- 'T.Text'.
cmdGeneralArgs :: T.Text -> (Message -> T.Text -> Client e ()) -> Command e
cmdGeneralArgs cmd f = cmdGeneralArgs' cmd $ \msg args -> True <$ f msg args

-- | A version of 'cmdGeneralArgs' that allows the command function to decide
-- whether the command was successful or not.
cmdGeneralArgs' :: T.Text -> (Message -> T.Text -> Client e Bool) -> Command e
cmdGeneralArgs' cmd = cmdMega' (pCmdGeneral cmd :: Parsec () T.Text T.Text)

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
cmdSpecificArgs' cmd name = cmdMega' (pCmdSpecific cmd name :: Parsec () T.Text T.Text)

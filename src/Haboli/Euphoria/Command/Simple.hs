{-# LANGUAGE OverloadedStrings #-}

module Haboli.Euphoria.Command.Simple
  ( cmdGeneral
  , cmdSpecific
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

cmdGeneral :: T.Text -> (Message -> T.Text -> Client e ()) -> Command e
cmdGeneral cmd = cmdMega $ pCmdGeneral cmd

pCmdSpecific :: T.Text -> T.Text -> Parser T.Text
pCmdSpecific cmd name = pCmd cmd *> space1 *> pNick name *> space *> pUntilEof

cmdSpecific :: T.Text -> T.Text -> (Message -> T.Text -> Client e ()) -> Command e
cmdSpecific name cmd = cmdMega $ pCmdSpecific cmd name

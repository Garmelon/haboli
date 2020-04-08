{-# LANGUAGE OverloadedStrings #-}

module Haboli.Euphoria.Command.Simple
  ( cmdGeneral
  , cmdGeneral'
  , cmdSpecific
  , cmdSpecific'
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

pWithoutArgs :: Parser T.Text -> Parser ()
pWithoutArgs p = do
  args <- p
  guard $ T.null args

cmdGeneral :: T.Text -> (Message -> Client e ()) -> Command e
cmdGeneral cmd f = cmdMega (pWithoutArgs $ pCmdGeneral cmd) $ \msg _ -> f msg

cmdGeneral' :: T.Text -> (Message -> T.Text -> Client e ()) -> Command e
cmdGeneral' cmd = cmdMega $ pCmdGeneral cmd

cmdSpecific :: T.Text -> T.Text -> (Message -> Client e ()) -> Command e
cmdSpecific cmd name f =
  cmdMega (pWithoutArgs $ pCmdSpecific cmd name) $ \msg _ -> f msg

cmdSpecific' :: T.Text -> T.Text -> (Message -> T.Text -> Client e ()) -> Command e
cmdSpecific' cmd name = cmdMega $ pCmdSpecific cmd name

{-# LANGUAGE OverloadedStrings #-}

module Haboli.Euphoria.Botrulez
  ( botrulezPingGeneral
  , botrulezPingSpecific
  , botrulezHelpGeneral
  , botrulezHelpSpecific
  , botrulezUptimeSpecific
  , botrulezKillSpecific
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Text                      as T
import           Data.Time

import           Haboli.Euphoria.Api
import           Haboli.Euphoria.Client
import           Haboli.Euphoria.Command
import           Haboli.Euphoria.Command.Simple
import           Haboli.Euphoria.Util

botrulezPingGeneral :: Command e
botrulezPingGeneral = cmdGeneral "ping" $ \msg ->
  void $ reply msg "Pong!"

botrulezPingSpecific :: T.Text -> Command e
botrulezPingSpecific name = cmdSpecific "ping" name $ \msg ->
  void $ reply msg "Pong!"

botrulezHelpGeneral :: T.Text -> Command e
botrulezHelpGeneral help = cmdGeneral "help" $ \msg ->
  void $ reply msg help

botrulezHelpSpecific :: T.Text -> T.Text -> Command e
botrulezHelpSpecific name help = cmdSpecific "help" name $ \msg ->
  void $ reply msg help

botrulezUptimeSpecific :: T.Text -> UTCTime -> Command e
botrulezUptimeSpecific name since = cmdSpecific "uptime" name $ \msg -> do
  now <- liftIO getCurrentTime
  let delta = diffUTCTime now since
  void $ reply msg $ mconcat
    [ "/me has been up since "
    , formatUTCTime since
    , " UTC ("
    , formatNominalDiffTime delta
    , ")"
    ]

botrulezKillSpecific :: T.Text -> Command T.Text
botrulezKillSpecific name = cmdSpecific "kill" name $ \msg -> do
  void $ reply msg "/me dies"
  throw $ "I was killed by " <> svNick (msgSender msg)

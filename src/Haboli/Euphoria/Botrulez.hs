{-# LANGUAGE OverloadedStrings #-}

-- | This module implements a few commands defined in the
-- [botrulez](https://github.com/jedevc/botrulez). If you need more advanced
-- behaviour, it should be pretty easy to reimplement the commands as necessary.

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

-- | @'botrulezPingGeneral'@ replies to commands of the form @!ping@ with
-- @Pong!@.
botrulezPingGeneral :: Command e
botrulezPingGeneral = cmdGeneral "ping" $ \msg ->
  void $ reply msg "Pong!"

-- | @'botrulezPingSpecific' nick@ replies to commands of the form @!ping
-- \@nick@ with @Pong!@.
botrulezPingSpecific :: T.Text -> Command e
botrulezPingSpecific name = cmdSpecific "ping" name $ \msg ->
  void $ reply msg "Pong!"

-- | @'botrulezHelpGeneral' helpText@ replies to commands of the form @!help@
-- with @helpText@.
botrulezHelpGeneral :: T.Text -> Command e
botrulezHelpGeneral help = cmdGeneral "help" $ \msg ->
  void $ reply msg help

-- | @'botrulezHelpSpecific' nick helpText@ replies to commands of the form
-- @!help \@nick@ with @helpText@.
botrulezHelpSpecific :: T.Text -> T.Text -> Command e
botrulezHelpSpecific name help = cmdSpecific "help" name $ \msg ->
  void $ reply msg help

-- | @'botrulezUptimeSpecific' nick startTime@ replies to commands of the form
-- @!uptime \@nick@ with the time since @startTime@.
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

-- | @'botrulezKillSpecific' nick@ replies to commands of the form @!kill
-- \@nick@ with @/me dies@. It then throws an exception.
botrulezKillSpecific :: T.Text -> Command T.Text
botrulezKillSpecific name = cmdSpecific "kill" name $ \msg -> do
  void $ reply msg "/me dies"
  throw $ "I was killed by " <> svNick (msgSender msg)

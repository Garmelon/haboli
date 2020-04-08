{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains a few utility functions that don't deserve their own
-- modules.

module Haboli.Euphoria.Util
  (
  -- * Events
    respondingToPing
  , respondingToBounce
  , respondingToBounce'
  , untilConnected
  , untilConnected'
  -- * Nick
  , nickMention
  , nickNormalize
  , nickEqual
  -- * Time formatting
  , formatUTCTime
  , formatNominalDiffTime
  ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Char
import           Data.Function
import qualified Data.Set                  as Set
import qualified Data.Text                 as T
import           Data.Time

import           Haboli.Euphoria.Api
import           Haboli.Euphoria.Client

{- Events -}

-- | Respond to 'EventPing's according to the euphoria documentation (see
-- <http://api.euphoria.io/#ping-event>). Passes through all events unmodified.
--
-- This utility function is meant to be wrapped directly or indirectly around
-- 'nextEvent':
--
-- > event <- respondingToPing nextEvent
respondingToPing :: Client e Event -> Client e Event
respondingToPing getEvent = do
  event <- getEvent
  case event of
    EventPing e -> pingReply (pingTime e)
    _           -> pure ()
  pure event

-- | Respond to 'EventBounce's according to the euphoria documentation. If no
-- password is provided but an 'EventBounce' is encountered, throw a 'T.Text'
-- exception.
--
-- This utility function is meant to be wrapped directly or indirectly around
-- 'nextEvent':
--
-- > event <- respondingToBounce (Just passwd) nextEvent
respondingToBounce :: Maybe T.Text -> Client T.Text Event -> Client T.Text Event
respondingToBounce = respondingToBounce' id

-- | A variant of 'respondingToBounce' that allows wrapping the exception into a
-- custom type.
respondingToBounce' :: (T.Text -> e) -> Maybe T.Text -> Client e Event -> Client e Event
respondingToBounce' onError mPasswd getEvent = do
  event <- getEvent
  case event of
    EventBounce e
      | Passcode `elem` bounceAuthOption e -> case mPasswd of
          Nothing -> throw $ onError "Password required but no password given"
          Just passwd -> do
            response <- auth passwd
            case response of
              Left msg -> throw $ onError $ "Could not authenticate: " <> msg
              Right () -> pure ()
    _ -> pure ()
  pure event

-- | Receive events until both an 'EventHello' and 'EventSnapshot' were
-- received, then return those. Throw a 'T.Text' exception if an invalid 'Event'
-- was encountered. Valid events are 'EventPing', 'EventBounce', 'EventHello'
-- and 'EventSnapshot'.
untilConnected :: Client T.Text Event -> Client T.Text (HelloEvent, SnapshotEvent)
untilConnected = untilConnected' id

-- | A variant of 'untilConnected' that allows wrapping the exception into a
-- custom type.
untilConnected' :: (T.Text -> e) -> Client e Event -> Client e (HelloEvent, SnapshotEvent)
untilConnected' onError getEvent = evalStateT helper (Nothing, Nothing)
  where
    helper = do
      event <- lift getEvent
      case event of
        EventPing _     -> pure ()
        EventBounce _   -> pure ()
        EventHello e    -> modify $ \(_, s) -> (Just e, s)
        EventSnapshot e -> modify $ \(h, _) -> (h, Just e)
        _ -> lift $ throw $ onError "Received disallowed packet while connecting"
      receivedEvents <- get
      case receivedEvents of
        (Just h, Just s) -> pure (h, s)
        _                -> helper

{- Nick -}

-- | Modify a nick such that — when prepended with an @\@@ — it will (hopefully)
-- ping the person with that nick on euphoria.
nickMention :: T.Text -> T.Text
nickMention name
  | T.length name > 1 = T.filter isMentionChar name
  | otherwise         = name
  where
    isMentionChar c = not $ isSpace c || c `Set.member` terminatingChars
    terminatingChars = Set.fromList ",.!?;&<'\""

-- | Normalize nicks (for nick comparison purposes) by removing all space
-- characters and converting the rest into a case-insensitive representation.
nickNormalize :: T.Text -> T.Text
nickNormalize name
  | T.length name > 1 = T.toCaseFold $ T.filter (not . isSpace) name
  | otherwise         = T.toCaseFold name

-- | Check two nicks for equality by comparing their normalized versions.
nickEqual :: T.Text -> T.Text -> Bool
nickEqual = (==) `on` nickNormalize

{- Time formatting -}

-- | Convert a 'UTCTime' into the format @yyyy-mm-dd HH:MM:SS@.
formatUTCTime :: UTCTime -> T.Text
formatUTCTime t = T.pack $ formatTime defaultTimeLocale "%F %T" t

-- | Convert a 'NominalDiffTime' into the format @[[[\<days\>d ]\<hours\>h
-- ]\<minutes\>m ]\<seconds\>s@ where the square brackets denote optional parts.
-- Only those parts required to fully display the time span are output. If the
-- 'NominalDiffTime' is negative, a @-@ is prefixed.
formatNominalDiffTime :: NominalDiffTime -> T.Text
formatNominalDiffTime t = (sign <>) $ T.intercalate " " $ map T.pack $ if
  | days    /= 0 -> [fDays, fHours, fMinutes, fSeconds]
  | hours   /= 0 -> [       fHours, fMinutes, fSeconds]
  | minutes /= 0 -> [               fMinutes, fSeconds]
  | otherwise    -> [                         fSeconds]
  where
    diffSeconds = round $ nominalDiffTimeToSeconds t :: Integer
    sign = if diffSeconds < 0 then "-" else ""
    totalSeconds = abs diffSeconds
    (days,    secondsAfterDays)  = totalSeconds      `quotRem` (60 * 60 * 24)
    (hours,   secondsAfterHours) = secondsAfterDays  `quotRem` (60 * 60)
    (minutes, seconds)           = secondsAfterHours `quotRem`  60
    fDays    = show days    ++ "d"
    fHours   = show hours   ++ "h"
    fMinutes = show minutes ++ "m"
    fSeconds = show seconds ++ "s"

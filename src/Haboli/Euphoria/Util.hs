{-# LANGUAGE OverloadedStrings #-}

module Haboli.Euphoria.Util
  ( formatUTCTime
  , formatNominalDiffTime
  -- * Events
  , respondingToPing
  , respondingToBounce
  , respondingToBounce'
  , untilConnected
  , untilConnected'
  -- * Nick
  , nickMention
  , nickNormalize
  , nickEqual
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

formatUTCTime :: UTCTime -> T.Text
formatUTCTime t = T.pack $ formatTime defaultTimeLocale "%F %T" t

formatNominalDiffTime :: NominalDiffTime -> T.Text
formatNominalDiffTime t = T.intercalate " " $ map T.pack $ concat
  [ [show days    ++ "d" | days    /= 0]
  , [show hours   ++ "h" | hours   /= 0]
  , [show minutes ++ "m" | minutes /= 0]
  , [show seconds ++ "s"]
  ]
  where
    totalSeconds = round $ nominalDiffTimeToSeconds t :: Integer
    (days,    secondsAfterDays)  = totalSeconds      `quotRem` (60 * 60 * 24)
    (hours,   secondsAfterHours) = secondsAfterDays  `quotRem` (60 * 60)
    (minutes, seconds)           = secondsAfterHours `quotRem`  60

{- Events -}

-- | Respond to 'EventPing's according to the documentation (see
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

respondingToBounce :: Maybe T.Text -> Client T.Text Event -> Client T.Text Event
respondingToBounce = respondingToBounce' id

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

untilConnected :: Client T.Text Event -> Client T.Text (HelloEvent, SnapshotEvent)
untilConnected = untilConnected' id

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

nickMention :: T.Text -> T.Text
nickMention name
  | T.length name > 1 = T.filter isMentionChar name
  | otherwise         = name
  where
    isMentionChar c = not $ isSpace c || c `Set.member` terminatingChars
    terminatingChars = Set.fromList ",.!?;&<'\""

nickNormalize :: T.Text -> T.Text
nickNormalize name
  | T.length name > 1 = T.toCaseFold $ T.filter (not . isSpace) name
  | otherwise         = T.toCaseFold name

nickEqual :: T.Text -> T.Text -> Bool
nickEqual = (==) `on` nickNormalize

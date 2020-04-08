{-# LANGUAGE OverloadedStrings #-}

module Haboli.Euphoria.Listing
  ( Listing
  , newListing
  , self
  , others
  , updateOwnNick
  , preferNick
  , updateFromList
  , updateFromEvent
  ) where

import qualified Data.Map.Strict        as Map
import           Data.Maybe
import qualified Data.Text              as T

import           Haboli.Euphoria.Api
import           Haboli.Euphoria.Client

data Listing = Listing
  { lsSelf   :: SessionView
  , lsOthers :: Map.Map UserId SessionView
  } deriving (Show)

othersFromList :: [SessionView] -> Map.Map UserId SessionView
othersFromList sessions = Map.fromList [(svId sv, sv) | sv <- sessions]

newListing :: (HelloEvent, SnapshotEvent) -> Listing
newListing (h, s) = Listing
  { lsSelf   = helloSessionView h
  , lsOthers = othersFromList $ snapshotListing s
  }

self :: Listing -> SessionView
self = lsSelf

others :: Listing -> Map.Map UserId SessionView
others = lsOthers

updateOwnNick :: T.Text -> Listing -> Listing
updateOwnNick name listing = listing{lsSelf = (lsSelf listing){svNick = name}}

preferNick :: T.Text -> Listing -> Client e Listing
preferNick name listing
  | name == svNick (self listing) = pure listing
  | otherwise = do
      (_, newNick) <- nick name
      pure $ updateOwnNick newNick listing

updateFromList :: [SessionView] -> Listing -> Listing
updateFromList sessions listing =
  let ownId = svId $ lsSelf listing
      others' = othersFromList sessions
      newSelf = fromMaybe (lsSelf listing) $ others' Map.!? ownId
      newOthers = Map.filterWithKey (\k _ -> k /= ownId) others'
  in  Listing newSelf newOthers

onJoin :: SessionView -> Listing -> Listing
onJoin sv listing = listing{lsOthers = Map.insert (svId sv) sv $ lsOthers listing}

onPart :: SessionView -> Listing -> Listing
onPart sv listing = listing{lsOthers = Map.delete (svId sv) $ lsOthers listing}

updateFromEvent :: Event -> Listing -> Listing
updateFromEvent (EventJoin e) listing = onJoin (joinSession e) listing
updateFromEvent (EventPart e) listing = onPart (partSession e) listing
updateFromEvent (EventNetwork e) listing | networkType e == "partition" =
  let sId = networkServerId e
      sEra = networkServerEra e
      isAffected sv = svServerId sv == sId && svServerEra sv == sEra
      others' = Map.filter (not . isAffected) $ lsOthers listing
  in  listing{lsOthers = others'}
updateFromEvent _ listing = listing


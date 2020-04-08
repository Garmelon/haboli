{-# LANGUAGE OverloadedStrings #-}

-- | A 'Listing' helps keep track of a bot's own 'SessionView' as well as all
-- other clients connected to a room. It must be kept up-to-date manually.

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

-- | A listing contains a bot's own 'SessionView' (accessible via 'self') and a
-- map of all other clients currently connected to the room (accessible via
-- 'others'). The latter never includes the bot itself.
data Listing = Listing
  { lsSelf   :: SessionView
  , lsOthers :: Map.Map UserId SessionView
  } deriving (Show)

othersFromList :: [SessionView] -> Map.Map UserId SessionView
othersFromList sessions = Map.fromList [(svId sv, sv) | sv <- sessions]

-- | Create a new 'Listing' based on a 'HelloEvent' and a 'SnapshotEvent'.
newListing :: (HelloEvent, SnapshotEvent) -> Listing
newListing (h, s) = Listing
  { lsSelf   = helloSessionView h
  , lsOthers = othersFromList $ snapshotListing s
  }

-- | The 'SessionView' describing the bot itself.
self :: Listing -> SessionView
self = lsSelf

-- | The 'SessionView's describing the other clients connected to the current
-- room. Does not include the bot's own 'SessionView' (use 'self' to access
-- that).
others :: Listing -> Map.Map UserId SessionView
others = lsOthers

-- | Set the bot's own nick to a new nick.
updateOwnNick :: T.Text -> Listing -> Listing
updateOwnNick name listing = listing{lsSelf = (lsSelf listing){svNick = name}}

-- | Set the bot's nick and update the 'Listing' with the server's reply in one
-- go.
preferNick :: T.Text -> Listing -> Client e Listing
preferNick name listing
  | name == svNick (self listing) = pure listing
  | otherwise = do
      (_, newNick) <- nick name
      pure $ updateOwnNick newNick listing

-- | Update a 'Listing' from a list of sessions currently connected to the room.
-- Afterwards, the 'Listing' will contain only those sessions present in the
-- list.
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

-- | Update a 'Listing' based on an 'Event'. Follows the euphoria documentation
-- for 'JoinEvent', 'PartEvent' and 'NetworkEvent'.
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


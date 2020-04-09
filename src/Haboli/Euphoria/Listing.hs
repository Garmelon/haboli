{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | A 'Listing' helps keep track of a bot's own 'SessionView' as well as all
-- other clients connected to a room. It must be kept up-to-date manually.

module Haboli.Euphoria.Listing
  ( Listing(..)
  , lsSelfL
  , lsOthersL
  , newListing
  , updateOwnNick
  , preferNick
  , preferNickVia
  , updateFromList
  , updateFromListVia
  , updateFromEvent
  , updateFromEventVia
  ) where

import           Control.Concurrent
import           Control.Monad.IO.Class
import qualified Data.Map.Strict        as Map
import           Data.Maybe
import qualified Data.Text              as T
import           Lens.Micro

import           Haboli.Euphoria.Api
import           Haboli.Euphoria.Client
import           Haboli.Euphoria.Lens

-- | A listing contains a bot's own 'SessionView' (accessible via 'lsSelf') and a
-- map of all other clients currently connected to the room (accessible via
-- 'lsOthers'). The latter never includes the bot itself.
data Listing = Listing
  { lsSelf   :: SessionView
  -- ^ The 'SessionView' describing the bot itself.
  , lsOthers :: Map.Map UserId SessionView
  -- ^ The 'SessionView's describing the other clients connected to the current
  -- room. Does not include the bot's own 'SessionView' (use 'lsSelf' to access
  -- that).
  } deriving (Show)

makeLensesL ''Listing

othersFromList :: [SessionView] -> Map.Map UserId SessionView
othersFromList sessions = Map.fromList [(svId sv, sv) | sv <- sessions]

-- | Create a new 'Listing' based on a 'HelloEvent' and a 'SnapshotEvent'.
newListing :: (HelloEvent, SnapshotEvent) -> Listing
newListing (h, s) = Listing
  { lsSelf   = helloSessionView h
  , lsOthers = othersFromList $ snapshotListing s
  }

-- | Set the bot's own nick to a new nick.
updateOwnNick :: T.Text -> Listing -> Listing
updateOwnNick name = lsSelfL . svNickL .~ name

-- | Set the bot's nick and update the 'Listing' with the server's reply in one
-- go.
preferNick :: T.Text -> Listing -> Client e Listing
preferNick name listing
  | name == listing ^. lsSelfL . svNickL = pure listing
  | otherwise = do
      (_, newNick) <- nick name
      pure $ updateOwnNick newNick listing

-- | Like 'preferNick', but updates a 'Listing' inside a data type inside an
-- 'MVar'.
preferNickVia :: Lens' a Listing -> MVar a -> T.Text -> Client e ()
preferNickVia field mvar name = do
  a <- liftIO $ takeMVar mvar
  listing' <- preferNick name $ a ^. field
  let a' = a & field .~ listing'
  liftIO $ putMVar mvar a'

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

-- | Like 'updateFromList', but updates a 'Listing' inside a data type inside an
-- 'MVar'.
updateFromListVia :: Lens' a Listing -> MVar a -> [SessionView] -> Client e ()
updateFromListVia field mvar list =
  liftIO $ modifyMVar_ mvar $ pure . (field %~ updateFromList list)

-- | Update a 'Listing' based on an 'Event'. Follows the euphoria documentation
-- for 'JoinEvent', 'PartEvent' and 'NetworkEvent'.
updateFromEvent :: Event -> Listing -> Listing
updateFromEvent (EventJoin e) listing =
  let sv = joinSession e
  in  listing & lsOthersL %~ Map.insert (svId sv) sv
updateFromEvent (EventPart e) listing =
  let sv = partSession e
  in  listing & lsOthersL %~ Map.delete (svId sv)
updateFromEvent (EventNetwork e) listing | networkType e == "partition" =
  let sId = networkServerId e
      sEra = networkServerEra e
      isAffected sv = svServerId sv == sId && svServerEra sv == sEra
  in  listing & lsOthersL %~ Map.filter (not . isAffected)
updateFromEvent _ listing = listing

-- | Like 'updateFromEvent', but updates a 'Listing' inside a data type inside
-- an 'MVar'.
updateFromEventVia :: Lens' a Listing -> MVar a -> Event -> Client e ()
updateFromEventVia field mvar event =
  liftIO $ modifyMVar_ mvar $ pure . (field %~ updateFromEvent event)

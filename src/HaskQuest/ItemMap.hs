module HaskQuest.ItemMap where

import HaskQuest.Item (ItemID, Item (..))
import HaskQuest.Room (RoomID)

import qualified Data.Map as Map

{-
Pairs ItemIDs with their appropriate item, whether that item may be retrieved by
the player, and where that item currently exists.
-}
data ItemInfo = ItemInfo
    { item          :: Item             -- The actual item.
    , retrievable   :: Bool             -- Can it be picked up by the player?
    , existsIn      :: Maybe RoomID     -- What room is it in? (Assumed to be
                                        --  in inventory if Nothing.)
    } deriving (Show)

-- A mapping between ItemIDs and ItemInfos
type ItemMap = Map.Map ItemID ItemInfo

emptyItemMap :: ItemMap
emptyItemMap = Map.empty

addItemToMap :: Item -> Bool -> ItemMap -> ItemMap
addItemToMap item itemRetr = Map.insert (name item) (ItemInfo item itemRetr Nothing)

addItemInfoToMap :: ItemID -> ItemInfo -> ItemMap -> ItemMap
addItemInfoToMap = Map.insert

lookupItemInfo :: ItemID -> ItemMap -> Maybe ItemInfo
lookupItemInfo = Map.lookup

lookupItem :: ItemID -> ItemMap -> Maybe Item
lookupItem itemID im = case mio of
    Just io
        -> Just (item io)
    Nothing
        -> Nothing
    where mio = lookupItemInfo itemID im

itemIsRetrievable :: ItemID -> ItemMap -> Bool
itemIsRetrievable itemID im = case mio of
    Just io
        -> retrievable io
    Nothing
        -> False
    where mio = lookupItemInfo itemID im

moveItemInMap :: ItemID -> Maybe RoomID -> ItemMap -> ItemMap
moveItemInMap itemID mri im = case mio of
    Just io
        -> addItemInfoToMap itemID (io { existsIn = mri }) im
    Nothing
        -> im
    where mio = lookupItemInfo itemID im

-- itemExistsInMap :: ItemID -> ItemMap -> Bool
-- itemExistsInMap itemID im = case mio of
--     Just _  -> True
--     Nothing -> False
--     where mio = lookupItemInfo itemID im

itemString :: ItemMap -> ItemID -> String
itemString im itemID = case mi of
    Just i
        -> name i
    Nothing
        -> ""
    where mi = lookupItem itemID im

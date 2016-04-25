module HaskQuest.Engine where

import HaskQuest.Item
import HaskQuest.ItemMap
import HaskQuest.Room
import HaskQuest.RoomMap

data Engine = Engine
    { currentRoom   :: Room
    , prevRoom      :: Maybe RoomID
    , roomMap       :: RoomMap
    , itemMap       :: ItemMap
    , inventory     :: [ItemID]
    } deriving (Show)

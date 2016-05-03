module HaskQuest.Game.Engine 
    ( Engine (..)
    , module HaskQuest.Game.Objects
    ) where

import HaskQuest.Game.Objects

data Engine = Engine
    { currentRoom   :: RoomID
    , previousRoom  :: Maybe RoomID
    , roomMap       :: RoomMap
    , itemMap       :: ItemMap
    , inventory     :: [ItemID]
    } deriving (Show)

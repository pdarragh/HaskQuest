module HaskQuest.Game.Engine 
    ( Engine (..)
    , module HaskQuest.Game.Objects
    ) where

import HaskQuest.Game.Objects

data Engine = Engine
    { currentRoom   :: Room
    , prevRoom      :: Maybe RoomID
    , roomMap       :: RoomMap
    , itemMap       :: ItemMap
    , inventory     :: [ItemID]
    } deriving (Show)

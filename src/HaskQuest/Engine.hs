module HaskQuest.Engine where

import HaskQuest.Item (Item (..))
import HaskQuest.Room (RoomID, Room (..), Exit (..))

import qualified Data.Map as Map

type RoomMap = Map.Map RoomID Room

emptyRoomMap :: RoomMap
emptyRoomMap = Map.empty

addRoomToMap :: Room -> RoomMap -> RoomMap
addRoomToMap r = Map.insert (roomID r) r

data Engine = Engine
    { currentRoom   :: Room
    , prevRoom      :: Maybe RoomID
    , rooms         :: RoomMap
    , inventory     :: [Item]
    } deriving (Show)

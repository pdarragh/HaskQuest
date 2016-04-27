module HaskQuest.Game.Objects.RoomMap where

import HaskQuest.Game.Objects.Room

import qualified Data.Map as Map

type RoomMap = Map.Map RoomID Room

emptyRoomMap :: RoomMap
emptyRoomMap = Map.empty

addRoomToMap :: Room -> RoomMap -> RoomMap
addRoomToMap r = Map.insert (roomID r) r

lookupRoom :: RoomID -> RoomMap -> Maybe Room
lookupRoom = Map.lookup

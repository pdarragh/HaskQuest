module HaskQuest.GameStateMonad where

import HaskQuest.Engine
import HaskQuest.State
import HaskQuest.Room
import HaskQuest.Item

import Data.List (nub)

import qualified Data.Map as Map

import Prelude hiding (stateError)

{-
The intersection of the game engine with the simple state monad. The
implementation herein allows for easy modification of the game state without
having to hold onto an Engine record and pass it around in psuedo-imperative
style.
-}

-- The connection between the State monad and the game engine.
type GameStateM a = State Engine a

-- Change the room to a different room.
-- If the desired room is not specified within the game engine's map of rooms,
-- and error is produced instead.
setRoom :: RoomID -> GameStateM ()
setRoom s = do
    (Engine r p rs i) <- get
    case roomIDFromExits (exits r) s of
        Just roomid
            -> case lookupRoom roomid rs of
                Just room
                    -> put (Engine room (Just (roomID r)) rs i)
                Nothing
                    -> stateError "Implementation error: no matching RoomID."
        Nothing
            -> stateError ("No such room: " ++ s)

-- Attempt to return to the previous room.
-- Depends on whether that room is defined in the current room's list of exits.
setRoomPrev :: GameStateM ()
setRoomPrev = do
    (Engine r p rs i) <- get
    case roomFromExits rs (exits r) p of
        Just room
            -> put (Engine room (Just (roomID r)) rs i)
        Nothing
            -> stateError "Cannot go back!"

-- Install an additional room into the engine's mapping.
addRoom :: Room -> GameStateM ()
addRoom new = do
    (Engine r p rs i) <- get
    let newRS = Map.insert (roomID new) new rs
    put (Engine r p newRS i)

-- Add a new item to the player's inventory.
addItem :: Item -> GameStateM ()
addItem new = do
    (Engine r p rs i) <- get
    put (Engine r p rs (new:i))

-- Determines whether a given string is a valid RoomID in the list of exits.
roomIDFromExits :: [Exit] -> String -> Maybe RoomID
roomIDFromExits es s = if length matchedExits == 1
        then
            Just $ exitID (head matchedExits)
        else
            Nothing
    where
        matchedExits = nub $ filter (elem s . aliases) es

-- An alias for looking up rooms in the RoomMap by RoomID.
lookupRoom :: RoomID -> RoomMap -> Maybe Room
lookupRoom = Map.lookup

-- Attempts to acquire a room from the room map if it exists within a list of
-- exits.
roomFromExits :: RoomMap -> [Exit] -> Maybe RoomID -> Maybe Room
roomFromExits rs es mr = case mr of
    Just roomid
        -> case filtered of
            [e] -> lookupRoom roomid rs
            _   -> Nothing
            where filtered = filter ( (==) roomid . exitID) es
    Nothing
        -> Nothing

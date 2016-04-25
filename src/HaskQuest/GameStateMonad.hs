module HaskQuest.GameStateMonad where

import HaskQuest.Engine
import HaskQuest.Item
import HaskQuest.ItemMap
import HaskQuest.Room
import HaskQuest.RoomMap
import HaskQuest.State

import Data.List (nub, delete)

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
    (Engine r p rm im inv) <- get
    case roomIDFromExits (exits r) s of
        Just roomid
            -> case lookupRoom roomid rm of
                Just room
                    -> put (Engine room (Just (roomID r)) rm im inv)
                Nothing
                    -> stateError "Implementation error: no matching RoomID."
        Nothing
            -> stateError ("No such room: " ++ s)

-- Attempt to return to the previous room.
-- Depends on whether that room is defined in the current room's list of exits.
setRoomPrev :: GameStateM ()
setRoomPrev = do
    (Engine r p rm im inv) <- get
    case roomFromExits rm (exits r) p of
        Just room
            -> put (Engine room (Just (roomID r)) rm im inv)
        Nothing
            -> stateError "Cannot go back!"

-- Install an additional room into the engine's mapping.
addRoom :: Room -> GameStateM ()
addRoom new = do
    (Engine r p rm im inv) <- get
    let newRM = addRoomToMap new rm
    put (Engine r p newRM im inv)

-- Add a new key item to the player's inventory.
addNewItem :: Item -> GameStateM ()
addNewItem new = do
    (Engine r p rm im inv) <- get
    let io = ItemInfo new False Nothing
    let newIM = addItemInfoToMap (name new) io im
    put (Engine r p rm newIM inv)

-- Move an item from the current room to the player's inventory.
pickupItem :: ItemID -> GameStateM ()
pickupItem itemID = do
    (Engine r p rm im inv) <- get
    if elem itemID (items r)
        then moveItemToInventory itemID
        else stateError "No such item in room!"

-- Move an item from the inventory to the current room.
dropItem :: ItemID -> GameStateM ()
dropItem itemID = do
    (Engine r p rm im inv) <- get
    moveItemFromInventory itemID (roomID r)

-- Take an item out of the inventory and add it to the specified room.
moveItemFromInventory :: ItemID -> RoomID -> GameStateM ()
moveItemFromInventory itemID roomID = do
    (Engine r p rm im inv) <- get
    if elem itemID inv
        then do
            let newIM  = moveItemInMap itemID (Just roomID) im
            put (Engine r p rm newIM inv)
            removeItemFromInventory itemID
            addItemToRoom itemID roomID
        else
            stateError "No such item in inventory!"

-- Add an item to a specific room.
addItemToRoom :: ItemID -> RoomID -> GameStateM ()
addItemToRoom itemID roomID = do
    (Engine r p rm im inv) <- get
    case lookupRoom roomID rm of
        Just room
            -> do
                let newRoom = addItem room itemID
                    newRM   = addRoomToMap newRoom rm
                put (Engine r p newRM im inv)
        Nothing
            -> stateError "No such room!"

-- Put an item into the inventory from anywhere.
moveItemToInventory :: ItemID -> GameStateM ()
moveItemToInventory itemID = do
    (Engine r p rm im inv) <- get
    case lookupItemInfo itemID im of
        Just (ItemInfo _ _ (Just roomID))
            -- Item is in a room. Move it to inventory.
            -> moveItemToInventory' itemID roomID
        Just (ItemInfo _ _ Nothing)
            -- Item is already in inventory.
            -> put (Engine r p rm im inv)
        Nothing
            -> stateError "No such item!"

-- Take an item from a room and put it in the inventory.
moveItemToInventory' :: ItemID -> RoomID -> GameStateM ()
moveItemToInventory' itemID roomID = do
    removeItemFromRoom itemID roomID
    (Engine r p rm im inv) <- get
    let newIM  = moveItemInMap itemID (Just roomID) im
        newInv = itemID:inv
    put (Engine r p rm newIM newInv)

-- Move an item from its current location to another location.
moveItem :: ItemID -> Maybe RoomID -> GameStateM ()
moveItem itemID mri = do
    (Engine r p rm im inv) <- get
    -- Find out where item is right now.
    case lookupItemInfo itemID im of
        Just (ItemInfo _ _ (Just roomID))
            -- Item is in a room.
            -> do
                removeItemFromRoom itemID roomID
                moveItem' itemID mri
        Just (ItemInfo _ _ Nothing)
            -- Item is in inventory.
            -> do
                removeItemFromInventory itemID
                moveItem' itemID mri
        Nothing
            -- Item does not exist.
            -> stateError "No such item!"

-- Helper for moveItem. Assumes the item has been removed from its origin.
moveItem' :: ItemID -> Maybe RoomID -> GameStateM ()
moveItem' itemID mri = do
    (Engine r p rm im inv) <- get
    case mri of
        Just roomID
            -- Add item to room.
            -> addItemToRoom itemID roomID
        Nothing
            -- Add item to inventory.
            -> do
                let newInv = itemID:inv
                put (Engine r p rm im newInv)

-- Removes an item from a room (if that room exists).
removeItemFromRoom :: ItemID -> RoomID -> GameStateM ()
removeItemFromRoom itemID roomID = do
    (Engine r p rm im inv) <- get
    case lookupRoom roomID rm of
        Just room
            -> do
                let is = items room
                    newRM = addRoomToMap (room { items = delete itemID is }) rm
                put (Engine r p newRM im inv)
        Nothing
            -> stateError "No such room!"

-- Removes an item from inventory.
removeItemFromInventory :: ItemID -> GameStateM ()
removeItemFromInventory itemID = do
    (Engine r p rm im inv) <- get
    let newInv = delete itemID inv
    put (Engine r p rm im newInv)

-- Determines whether a given string is a valid RoomID in the list of exits.
roomIDFromExits :: [Exit] -> String -> Maybe RoomID
roomIDFromExits es s = if length matchedExits == 1
        then
            Just $ exitID (head matchedExits)
        else
            Nothing
    where
        matchedExits = nub $ filter (elem s . aliases) es

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

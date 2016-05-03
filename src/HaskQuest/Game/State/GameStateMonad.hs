module HaskQuest.Game.State.GameStateMonad
    ( GameStateM
    , getCurrentRoom
    , goToRoom
    , goBack
    , getItem
    , pickupItem
    , dropItem
    ) where

import HaskQuest.Game.Engine
import HaskQuest.Game.State.SimpleState

import Data.List (nub, delete)

{-
The intersection of the game engine with the simple state monad. The
implementation herein allows for easy modification of the game state without
having to hold onto an Engine record and pass it around in psuedo-imperative
style.
-}

-- The connection between the State monad and the game engine.
type GameStateM a = State Engine a

-- Take an item from the current room and move it into the inventory.
pickupItem :: ItemID -> GameStateM ()
pickupItem itemID = do
    e <- get
    cr <- getCurrentRoom
    rm <- getRoomMap
    if itemID `elem` items cr
        then
            let
                newItems = delete itemID (items cr)
                newRM = addRoomToMap (cr { items = newItems }) rm
                newInv = itemID : inventory e
            in
                put (e { roomMap = newRM, inventory = newInv })
        else stateError ("Cannot find " ++ itemID ++ " to pick up!")

-- Remove an item from the inventory and place it in the current room.
dropItem :: ItemID -> GameStateM ()
dropItem itemID = do
    e <- get
    cr <- getCurrentRoom
    rm <- getRoomMap
    if itemID `elem` inventory e
        then
            let
                newInv = delete itemID (inventory e)
                newItems = itemID : items cr
                newRM = addRoomToMap (cr { items = newItems }) rm
            in
                put (e { roomMap = newRM, inventory = newInv })
        else stateError ("No " ++ itemID ++ " in inventory!")

-- Try to look up the given item in the engine.
getItem :: ItemID -> GameStateM (Maybe Item)
getItem itemID = do
    e <- get
    return (lookupItem itemID (itemMap e))

-- Try to look up the ItemInfo for the given ID.
lookupItemInfoInEngine :: ItemID -> GameStateM (Maybe ItemInfo)
lookupItemInfoInEngine itemID = do
    e <- get
    im <- getItemMap
    return (lookupItemInfo itemID im)

-- Pulls the current ItemMap out of the engine.
getItemMap :: GameStateM ItemMap
getItemMap = do
    e <- get
    return (itemMap e)

-- Attempt to change the current room to a different room attached to the
-- current room.
goToRoom :: RoomID -> GameStateM ()
goToRoom alias = do
    e <- get
    cr <- getCurrentRoom
    exitID <- lookupCurrentRoomExitByAlias alias
    case exitID of
        Just validRoomID
            -> do
                maybeRoom <- lookupRoomInEngine validRoomID
                case maybeRoom of
                    Just room
                        -> put (e { currentRoom = roomID room, previousRoom = Just (roomID cr) })
                    Nothing
                        -> stateError "That location doesn't exist!"
        Nothing
            -> stateError ("Cannot go " ++ alias ++ ".")

-- Given an alias, determines whether that alias is a valid exit from the
-- current room.
lookupCurrentRoomExitByAlias :: String -> GameStateM (Maybe RoomID)
lookupCurrentRoomExitByAlias alias = do
    e <- get
    cr <- getCurrentRoom
    case filter (elem alias . aliases) (exits cr) of
        [e] -> return (Just (exitID e))
        _   -> return Nothing

-- Given a RoomID, attempts to discern whether there is an exit to that room
-- from the current room.
lookupCurrentRoomExitByID :: RoomID -> GameStateM (Maybe RoomID)
lookupCurrentRoomExitByID inputID = do
    e <- get
    cr <- getCurrentRoom
    case filter ( (==) inputID . exitID) (exits cr) of
        [e] -> return (Just (exitID e))
        _   -> return Nothing

-- Pulls the RoomMap out of the engine.
getRoomMap :: GameStateM RoomMap
getRoomMap = do
    e <- get
    return (roomMap e)

-- Attempt to return to the previous room.
-- Depends on whether that room is defined in the current room's list of exits.
goBack :: GameStateM ()
goBack = do
    e <- get
    let crID = currentRoom e
    case previousRoom e of
        Just prevRoomID
            -> do
                prev <- lookupCurrentRoomExitByID prevRoomID
                case prev of
                    Just realPrev
                        -> put (e { currentRoom = realPrev, previousRoom = Just crID })
                    Nothing
                        -> stateError "Cannot go back!"
        Nothing
            -> stateError "Cannot go back!"

-- Install an additional room into the engine's mapping.
addRoom :: Room -> GameStateM ()
addRoom room = do
    e <- get
    put (e { roomMap = addRoomToMap room (roomMap e) })

-- Returns the current room.
getCurrentRoom :: GameStateM Room
getCurrentRoom = do
    e <- get
    cr <- lookupRoomInEngine (currentRoom e)
    case cr of
        Just room
            -> return room
        Nothing
            -> error "Could not acquire current location."

-- Searches the engine to find a particular room.
lookupRoomInEngine :: RoomID -> GameStateM (Maybe Room)
lookupRoomInEngine roomID = do
    e <- get
    return (lookupRoom roomID (roomMap e))

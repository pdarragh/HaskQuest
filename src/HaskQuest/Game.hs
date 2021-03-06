module HaskQuest.Game
    ( startGame
    , module HaskQuest.Game.Engine
    ) where

import HaskQuest.Game.Engine
import HaskQuest.Game.Parser
import HaskQuest.Game.State

import System.Console.Readline
import Control.Monad (when)
import Data.List (intercalate)
import Prelude hiding (print)
import System.IO (hFlush, stdout)

{-
The GameAction describes the desired next action to be taken within the main
game loop.
-}
data GameAction
    = Continue          -- Process the current game state to proceed.
    | ShowInventory     -- Player wants to see inventory.
    | ShowDescription   -- Player wants to see room description.
    | InspectItem Item  -- Player wants to investigate an item.
    | Pickup ItemID     -- Player picked up an item.
    | DropItem ItemID   -- Player dropped an item.
    | UserError String  -- Player did something wrong.

-- Exported to allow a game to be played.
startGame :: Engine -> IO ()
startGame e = runGame e True

-- Main loop for the game. Deterines whether to print the current room's
-- description and then takes input from the player to decide what to do next.
runGame :: Engine -> Bool -> IO ()
runGame e desc = do
    when desc (showDescription e)
    action <- actionFromPlayer
    case runState (processPlayerAction action) e of
        Left err
            -- Something went wrong.
            -> do
                gameError err
                runGame e False
        Right (Just ga, e')
            -- The player has continued the game.
            -> do
                printout <- processGameAction ga e'
                runGame e' printout
        Right (Nothing, e')
            -- The player asked to quit the game.
            -> return ()

-- Converts a PlayerAction into a GameAction, while also advancing the game
-- state accordingly.
processPlayerAction :: PlayerAction -> GameStateM (Maybe GameAction)
processPlayerAction (Go s) = do
    -- Attempt to change rooms.
    goToRoom s
    return (Just Continue)
processPlayerAction Back = do
    -- Attempt to go back to the previous room.
    goBack
    return (Just Continue)
processPlayerAction Inventory =
    -- Show the current inventory.
    return (Just ShowInventory)
processPlayerAction Description =
    -- Show the current room's description.
    return (Just ShowDescription)
processPlayerAction (Inspect i) = do
    -- Attempt to get the description of the item.
    mi <- getItem i
    case mi of
        Just item
            -> return (Just (InspectItem item))
        Nothing
            -> return (Just (UserError ("No such item: " ++ i)))
processPlayerAction (Take i) = do
    -- Try to pick up the item.
    pickupItem i
    return (Just (Pickup i))
processPlayerAction (Drop i) = do
    -- Try to drop the item.
    dropItem i
    return (Just (DropItem i))
processPlayerAction Quit =
    -- Quit the game.
    return Nothing
processPlayerAction Invalid =
    -- The player did not give proper input.
    return (Just (UserError "Invalid action!"))

-- Takes a GameAction and an engine and does the IO so the player can know
-- what's going on. The return value describes whether the main game loop should
-- print out a new room description.
processGameAction :: GameAction -> Engine -> IO Bool
processGameAction Continue _ =
    -- Player moved to a new room; describe it!
    return True
processGameAction (Pickup i) e = do
    -- Player picked up an item.
    print ("You picked up: " ++ i)
    showInventory e
    return False
processGameAction (DropItem i) _ = do
    -- Player dropped an item.
    print ("You dropped: " ++ i)
    return False
processGameAction ShowInventory e = do
    -- Player wants to see inventory.
    showInventory e
    return False
processGameAction ShowDescription _ =
    -- Player wants to see room description.
    return True
processGameAction (InspectItem i) _ = do
    -- Player wants information about an item.
    showItem i
    return False
processGameAction (UserError err) _ = do
    -- Player must be shown what they did wrong.
    gameError err
    return False

-- Displays the player's current inventory.
showInventory :: Engine -> IO ()
showInventory e = do
    print ""
    print "You are carrying:"
    mapM_ (printListItem . itemString (itemMap e)) (inventory e)

-- Displays the room description to the player.
showDescription :: Engine -> IO ()
showDescription e = do
    let
        crID = currentRoom e
        mcr = lookupRoom crID (roomMap e)
    case mcr of
        Just cr
            -> do
                showDescription' cr
                if null (inventory e)
                    then print "Inventory: (Empty)"
                    else print $ "Inventory: " ++ intercalate ", " (map (itemString (itemMap e)) (inventory e))
        Nothing
            -> error "No current room."

showDescription' :: Room -> IO ()
showDescription' cr = do
    print ""
    print (description cr)
    print ""
    print ""
    print ("Room: " ++ roomName cr)

-- Displays the desired item's description
showItem :: Item -> IO ()
showItem item = print (inspect item)

-- When something goes wrong, outputs information to the player.
gameError :: String -> IO ()
gameError s = do
    putStr $ leader ++ "\n"
    putStrLn $ leader ++ " (!) " ++ s

-- Takes input from a player and produces a PlayerAction.
actionFromPlayer :: IO PlayerAction
actionFromPlayer = do
    putStr $ leader ++ "\n"
    -- putStr $ leader ++ ">> "
    -- hFlush stdout
    input <- readline (leader ++ ">> ")
    case input of
        Nothing
            -> do
                putStrLn ""
                return Quit
        Just line
            -> return $ parseChoice line

-- The generic call to output lines.
print :: String -> IO ()
print s = putStrLn $ leader ++ " " ++ s

-- Special output for list items.
printListItem :: String -> IO ()
printListItem = printListItemAtLevel 1

-- Output nested list items.
printListItemAtLevel :: Int -> String -> IO ()
printListItemAtLevel l s = putStrLn $ leader ++ " " ++ concat (replicate l "  ") ++ "* " ++ s

-- The string to prefix each line with.
leader :: String
leader = "||"

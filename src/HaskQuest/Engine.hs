module HaskQuest.Engine
    ( Engine (..)
    , startGame
    , RoomMap
    , emptyRoomMap
    , addRoom
    ) where

{- Module Imports -}
import HaskQuest.Parser
import HaskQuest.Item (Item (..))
import HaskQuest.Room (RoomID, Room (..), Exit (..))

import qualified HaskQuest.Item as Item
import qualified HaskQuest.Room as Room

{- Library Imports -}
import Data.List (nub, intercalate, maximumBy)
import Data.Maybe
import Data.Ord (comparing)
import System.IO (hFlush, stdout)

import Prelude hiding (print)

import qualified Data.Map as Map

{-
All of the game mechanics needed to actually run the game.

-}

type RoomMap = Map.Map RoomID Room

emptyRoomMap :: RoomMap
emptyRoomMap = Map.empty

data GameAction
    = RunEngine Engine
    | ShowInventory
    | ShowDescription
    | UserError String
    | SystemQuit

data Engine = Engine
    { currentRoom   :: Room
    , prevRoom      :: Maybe RoomID
    , rooms         :: RoomMap
    , inventory     :: [Item]
    } deriving (Show)

startGame :: Engine -> IO ()
startGame e = runGame e True

runGame :: Engine -> Bool -> IO ()
runGame e s = do
    successDisplayPrompt s e
    action <- gameStep e
    case action of
        (RunEngine e')
            -> runGame e' True
        ShowInventory
            -> do
                showInventory (inventory e)
                runGame e False
        ShowDescription
            -> do
                showDescription e
                runGame e False
        (UserError s)
            -> do
                gameError s
                runGame e False
        SystemQuit
            -> return ()

successDisplayPrompt :: Bool -> Engine -> IO ()
successDisplayPrompt s e = if s
    then
        showDescription e
    else
        return ()

gameStep :: Engine -> IO (GameAction)
gameStep e = do
    input <- promptUser e
    return $ actOnParse e $ parseChoice input

addRoom :: RoomMap -> Room -> RoomMap
addRoom m r = Map.insert (roomID r) r m

roomIDFromExits :: [Exit] -> String -> Maybe RoomID
roomIDFromExits es s = if length matchedExits == 1
        then
            Just $ exitID (head matchedExits)
        else
            Nothing
    where
        matchedExits = nub $ filter (elem s . aliases) es

lookupRoom :: RoomMap -> RoomID -> Maybe Room
lookupRoom rs i = Map.lookup i rs

roomFromExits :: RoomMap -> [Exit] -> Maybe RoomID -> Maybe Room
roomFromExits rs es mr = case mr of
    Just roomid
        -> case filtered of
            (e:[])  -> lookupRoom rs roomid
            _       -> Nothing
            where filtered = filter ( (==) roomid . exitID) es
    Nothing
        -> Nothing

actOnParse :: Engine -> PlayerAction -> GameAction
actOnParse (Engine r p rs i) action = case action of
    (Go s)
        -> case roomIDFromExits (exits r) s of
            Just roomid
                -> case (lookupRoom rs roomid) of
                    Just room
                        -> RunEngine (Engine room (Just (roomID r)) rs i)
                    Nothing
                        -> UserError "Implementation error: no matching RoomID."
            Nothing
                -> UserError "No such room!"
    Back
        -> case roomFromExits rs (exits r) p of
            Just room
                -> RunEngine (Engine room (Just (roomID r)) rs i)
            Nothing
                -> UserError "Cannot go back!"
    Inventory
        -> ShowInventory
    Description
        -> ShowDescription
    Quit
        -> SystemQuit
    Invalid
        -> UserError "Invalid action!"

showInventory :: [Item] -> IO ()
showInventory i = do
    print ""
    print "You are carrying:"
    mapM_ print $ map ( (++) "  * " . name) i

showDescription :: Engine -> IO ()
showDescription e = do
    print ""
    print (description $ currentRoom e)
    print ""
    print ""
    print ("Room: " ++ (Room.roomID $ currentRoom e))
    if null (inventory e)
        then print "Inventory: (Empty)"
        else print $ "Inventory: " ++ (intercalate ", " $ map Item.name (inventory e))

promptUser :: Engine -> IO (String)
promptUser e = do
    input <- prompt
    return input

gameError :: String -> IO ()
gameError s = do
    putStr $ leader ++ "\n"
    putStrLn $ leader ++ " (!) " ++ s

leader :: String
leader = "||"

print :: String -> IO ()
print s = do
    putStrLn $ leader ++ " " ++ s

prompt :: IO (String)
prompt = do
    putStr $ leader ++ "\n"
    putStr $ leader ++ ">> "
    hFlush stdout
    input <- getLine
    return input

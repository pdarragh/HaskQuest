module HaskQuest.Engine
    ( Engine (..)
    , runGame
    ) where

import HaskQuest.Parser
import qualified HaskQuest.Item as Item
import qualified HaskQuest.Room as Room
import HaskQuest.Item (Item (..))
import HaskQuest.Room (Room (..), Exit (..))

import Prelude hiding (print)

import Data.List (nub, intercalate)
import Data.Maybe

{-
All of the game mechanics needed to actually run the game.

-}

data GameAction
    = RunEngine Engine
    | UserError String
    | SystemQuit

data Engine = Engine
    { currentRoom :: Room
    , prevRoom    :: Maybe Room
    , inventory   :: [Item]
    } deriving (Show)

runGame :: Engine -> IO ()
runGame e = do
    action <- gameStep e
    case action of
        (RunEngine e')
            -> runGame e'
        (UserError s)
            -> do
                gameError s
                runGame e
        SystemQuit
            -> return ()

gameStep :: Engine -> IO (GameAction)
gameStep e = do
    input <- promptUser e
    return $ actOnParse e $ parseChoice input

actOnParse :: Engine -> PlayerAction -> GameAction
actOnParse (Engine r p i) action = case action of
    (Go s)
        -> if length matchedExits == 1
            then
                RunEngine (Engine (room $ head matchedExits) (Just r) i)
            else
                UserError "No such room!"
            where
                matchedExits = nub $ filter (elem s . aliases) (exits r)
    Back
        -> if isNothing p || null (filter ( (==) (fromJust p) . room) (exits r))
            then
                UserError "Cannot go back!"
            else
                RunEngine (Engine (fromJust p) (Just r) i)
    Quit
        -> SystemQuit

promptUser :: Engine -> IO (String)
promptUser e = do
    print ""
    print (description $ currentRoom e)
    print ""
    print ("Room: " ++ (Room.name $ currentRoom e))
    if null (inventory e)
        then print "Inventory: (Empty)"
        else print $ "Inventory: " ++ (intercalate ", " $ map Item.name (inventory e))
    input <- prompt
    return input

gameError :: String -> IO ()
gameError s = do
    putStrLn $ s ++ "\n"

leader :: String
leader = "||"

print :: String -> IO ()
print s = do
    putStrLn $ leader ++ " " ++ s

prompt :: IO (String)
prompt = do
    putStr $ leader ++ ">> "
    input <- getLine
    return input

module HaskQuest.Engine
    (
        Engine (..),
        runGame
    ) where

import HaskQuest.Item
import HaskQuest.Parser
import HaskQuest.Room

import Data.List (nub)
import Data.Maybe

{-
All of the game mechanics needed to actually run the game.

-}

data GameAction
    = RunEngine Engine
    | UserError String
    | SystemQuit

data Engine = Engine {
    currentRoom :: Room,
    prevRoom    :: Maybe Room,
    inventory   :: [Item]
} deriving (Show)

runGame :: Engine -> IO ()
runGame e = do
    action <- gameStep e
    case action of
        (RunEngine e') -> runGame e'
        (UserError s) -> do
            gameError s
            runGame e
        SystemQuit -> do
            putStrLn "Quitting..."
            return ()

gameStep :: Engine -> IO (GameAction)
gameStep e = do
    -- print (Engine r p i)
    print (currentRoom e)
    input <- promptUser
    return $ actOnParse e $ parseChoice input

actOnParse :: Engine -> PlayerAction -> GameAction
actOnParse (Engine r p i) action = case action of
    (Go s) -> if length matchedExits == 1 then
            RunEngine (Engine (room $ head matchedExits) (Just r) i)
        else
            UserError "No matching room!"
        where
            inlined = filter ( (==) s . inline) (exits r)
            aliased = filter (elem s . aliases) (exits r)
            matchedExits = nub $ inlined ++ aliased
    Back -> if isNothing p || null (filter ( (==) (fromJust p) . room) (exits r)) then
            UserError "Cannot go back!"
        else
            RunEngine (Engine (fromJust p) (Just r) i)
    Quit -> SystemQuit

promptUser :: IO (String)
promptUser = do
    putStr "\n> "
    input <- getLine
    return input

gameError :: String -> IO ()
gameError s = do
    putStrLn $ s ++ "\n"

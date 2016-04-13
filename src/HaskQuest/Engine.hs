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

data Engine = Engine {
    currentRoom :: Room,
    prevRoom    :: Maybe Room,
    inventory   :: [Item]
} deriving (Show)

runGame :: Engine -> IO ()
runGame e = do
    me <- gameStep e
    case me of
        Nothing -> runGame e
        Just e' -> runGame e'

gameStep :: Engine -> IO (Maybe Engine)
gameStep (Engine r p i) = do
    -- print (Engine r p i)
    print r
    input <- promptUser
    let action = parseChoice input
    case action of
        (Go s) -> if length matchedExits == 1 then
                wrapEngine (Engine (room $ head matchedExits) (Just r) i)
            else
                gameError "No matching room!"
            where
                inlined = filter ( (==) s . inline) (exits r)
                aliased = filter (elem s . aliases) (exits r)
                matchedExits = nub $ inlined ++ aliased
        Back -> if isNothing p || null (filter ( (==) (fromJust p) . room) (exits r)) then
                gameError "Cannot go back!"
            else
                wrapEngine (Engine (fromJust p) (Just r) i)

promptUser :: IO (String)
promptUser = do
    putStr "\n> "
    input <- getLine
    return input

gameError :: String -> IO (Maybe Engine)
gameError s = do
    putStrLn $ s ++ "\n"
    return Nothing

wrapEngine :: Engine -> IO (Maybe Engine)
wrapEngine e = return (Just e)

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
runGame (Engine r p i) = do
    print r
    input <- getLine
    let action = parseChoice input
    case action of
        (Go s) -> if length matchedExits == 1 then
                runGame (Engine (room $ head matchedExits) (Just r) i)
            else
                error "No matching room!"
            where
                inlined = filter ( (==) s . inline) (exits r)
                aliased = filter (elem s . aliases) (exits r)
                matchedExits = nub $ inlined ++ aliased
        Back -> if isNothing p then
                error "Cannot go back!"
            else
                runGame (Engine (fromJust p) (Just r) i)

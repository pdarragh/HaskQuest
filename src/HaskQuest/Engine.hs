module HaskQuest.Engine
    (
        Engine (..),
        startGame
    ) where

import HaskQuest.Item
import HaskQuest.Parser
import HaskQuest.Room

import Data.List (nub)

{-
All of the game mechanics needed to actually run the game.

-}

data Engine = Engine {
    rooms :: [Room]
} deriving (Show)

startGame :: Engine -> IO ()
startGame e
    | null (rooms e) = error "No rooms to run."
    | otherwise = runRoom $ head (rooms e)

runRoom :: Room -> IO ()
runRoom r = do
    print r
    input <- getLine
    let action = parseChoice input
    case action of
        (Go s) -> if length matchedExits == 1 then
                runRoom $ room $ head matchedExits
            else
                error "No matching room!"
            where
                inlined = filter ( (==) s . inline) (exits r)
                aliased = filter (elem s . aliases) (exits r)
                matchedExits = nub $ inlined ++ aliased

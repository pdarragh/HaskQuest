module HaskQuest.Engine
    (
        Engine
    ) where

import HaskQuest.Room
import HaskQuest.Item

{-
All of the game mechanics needed to actually run the game.

-}

data Engine = Engine {
    rooms :: [Room]
}



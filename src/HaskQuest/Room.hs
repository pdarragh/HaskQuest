module HaskQuest.Room
    (
        Exit,
        Room
    ) where

import HaskQuest.Item

import Data.List (intercalate)

{-
The description of a room in an adventure.

Rooms must have at least a name, description, and a list of exits (although you
may choose to leave the list empty). The name and description are strings, and
the list of exits is a list of pairs of strings to Rooms. The strings represent
the name to use when traveling to the other room.

-}

type Exit = (String, Room)

data Room = Room {
    name :: String,
    description :: String,
    items :: [Item],
    exits :: [Exit]
}

showExits :: [Exit] -> String
showExits [] = "There are no exits."
showExits (e:[]) = "There is an exit " ++ (fst e) ++ "."
showExits es = "There are exits " ++ (intercalate ", " (init (map fst es))) ++ " and " ++ (head (reverse (map fst es))) ++ "."

instance Show Room where
    show (Room _ d [] es) = d ++ "\n\n" ++ (showExits es)

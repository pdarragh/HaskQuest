module HaskQuest.Item
    (
        Item
    ) where

{-
The description of items in the game.

Items contain a name, a short description, and a long description.

* Name: how items will be described in inventory
* Short description: how items will be described when you enter a room
* Long description: how items will be described upon inspection

-}

data Item = Item {
    name :: String,
    short :: String,
    long :: String
}

instance Show Item where
    show (Item n _ _) = n

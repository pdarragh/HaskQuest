module HaskQuest.Item
    ( ItemID
    , Item (..)
    , emptyItem
    , setItemName
    , setItemInspect
    ) where

{-
An ItemID is used to uniquely identify a specific Item within a game.
-}
type ItemID = Int

{-
An Item is something the player can interact with. For now, the only supported
actions are to 'take' an Item and 'inspect' an Item.

- name
    The proper name of the Item, as it will appear in the inventory.
- inspect
    The description of the Item when the player decides to 'inspect' it.
-}
data Item = Item
    { name      :: String
    , inspect   :: String
    } deriving (Eq)

instance Show Item where
    show (Item n _) = n

emptyItem :: Item
emptyItem = Item "" ""

setItemName :: Item -> String -> Item
setItemName i n = i { name = n }

setItemInspect :: Item -> String -> Item
setItemInspect i s = i { inspect = s }

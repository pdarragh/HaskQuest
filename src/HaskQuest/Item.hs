module HaskQuest.Item
    ( Item (..)
    , emptyItem
    , setName
    , setInspect
    ) where

{-
An Item is something the player can interact with. For now, the only supported
actions are to 'take' an Item and 'inspect' an Item.

- name
    The proper name of the Item, as it will appear in the inventory.
- inspect
    The description of the Item when the player decides to 'inspect' it.
-}
data Item = Item
    { name    :: String
    , inspect :: String
    } deriving (Eq)

instance Show Item where
    show (Item n _) = n

emptyItem :: Item
emptyItem = Item "" ""

setName :: Item -> String -> Item
setName i n = i { name = n }

setInspect :: Item -> String -> Item
setInspect i s = i { inspect = s }

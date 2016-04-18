module HaskQuest.Room
    ( Exit (..)
    , emptyExit
    , setExitAliases
    , addExitAlias
    , setExitRoom
    , Room (..)
    , emptyRoom
    , setName
    , setDesc
    , setItems
    , addItem
    , setExits
    , addExit
    ) where

import HaskQuest.Item (Item)

{-
The Exit is a description of a way out of a Room. Each Exit leads to another
Room and has (potentially) multiple ways of being referenced.

- aliases
    The various ways the player can refer to the Exit when they attempt to leave
    through it.
- room
    The Room to which the Exit leads.

Your list of aliases should be pretty complete, as it defines how a user is able
to find their way out of a room. A user will not be able to go through your Exit
without utilizing one of the phrases in your list of aliases, so think it
through.
-}
data Exit = Exit
    { aliases   :: [String]
    , room      :: Room
    } deriving (Show, Eq)

emptyExit :: Exit
emptyExit = Exit [] emptyRoom

setExitAliases :: Exit -> [String] -> Exit
setExitAliases e as = e { aliases = as }

addExitAlias :: Exit -> String -> Exit
addExitAlias e a = e { aliases = a:(aliases e) }

setExitRoom :: Exit -> Room -> Exit
setExitRoom e r = e { room = r }

{-
The Room datatype describes a room within your world. Rooms consist of:

- name
    The formal name of the room. This might show up on a map or a HUD if one is
    ever implemented.
- description
    Text the player sees when they enter the room.
- items
    A list of Items that the player might be able to interact with.
- exits
    A list of Exits leading to other 'Room's that the player can go to.

You are responsible for writing an apt description of a Room. The description
should include references to all Exits and Items you want your users to know
about, as no information about them is given upfront otherwise.
-}
data Room = Room
    { name          :: String
    , description   :: String
    , items         :: [Item]
    , exits         :: [Exit]
    } deriving (Eq)

instance Show Room where
    show (Room _ d _ _) = d

emptyRoom :: Room
emptyRoom = Room "" "" [] []

setName :: Room -> String -> Room
setName r n = r { name = n }

setDesc :: Room -> String -> Room
setDesc r d = r { description = d }

setItems :: Room -> [Item] -> Room
setItems r is = r { items = is }

addItem :: Room -> Item -> Room
addItem r i = r { items = i:(items r) }

setExits :: Room -> [Exit] -> Room
setExits r es = r { exits = es }

addExit :: Room -> Exit -> Room
addExit r e = r { exits = e:(exits r) }

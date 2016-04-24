module HaskQuest.Room
    ( RoomID
    , Exit (..)
    , setExitAliases
    , addExitAlias
    , setExitRoomID
    , Room (..)
    , roomName
    , emptyRoom
    , setDesc
    , setExits
    , addExit
    , setRetrievables
    , addRetrievable
    , setUnretrievables
    , addUnretrievable
    ) where

import HaskQuest.Item (Item)

{-
A RoomID is used to uniquely identify a specific room within a game. Creating
this structure allows for 
-}
type RoomID = String

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
    , exitID    :: RoomID
    } deriving (Show, Eq)

setExitAliases :: Exit -> [String] -> Exit
setExitAliases e as = e { aliases = as }

addExitAlias :: Exit -> String -> Exit
addExitAlias e a = e { aliases = a:aliases e }

setExitRoomID :: Exit -> RoomID -> Exit
setExitRoomID e i = e { exitID = i }

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
    { roomID            :: RoomID
    , description       :: String
    , exits             :: [Exit]
    , retrievables      :: [Item]
    , unretrievables    :: [Item]
    } deriving (Eq)

instance Show Room where
    show (Room _ d _ _ _) = d

roomName :: Room -> String
roomName r = roomID r

emptyRoom :: Room
emptyRoom = Room "Empty" "An empty room with no way out." [] [] []

setID :: Room -> RoomID -> Room
setID r i = r { roomID = i }

setDesc :: Room -> String -> Room
setDesc r d = r { description = d }

setExits :: Room -> [Exit] -> Room
setExits r es = r { exits = es }

addExit :: Room -> Exit -> Room
addExit r e = r { exits = e:exits r }

setRetrievables :: Room -> [Item] -> Room
setRetrievables r is = r { retrievables = is }

addRetrievable :: Room -> Item -> Room
addRetrievable r i = r { retrievables = i:retrievables r }

setUnretrievables :: Room -> [Item] -> Room
setUnretrievables r is = r { unretrievables = is }

addUnretrievable :: Room -> Item -> Room
addUnretrievable r i = r { unretrievables = i:unretrievables r }

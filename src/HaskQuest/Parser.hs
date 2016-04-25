module HaskQuest.Parser
    ( PlayerAction (..)
    , parseChoice
    )
    where

import qualified Data.Char (toLower)

{-
For handling user input and then doing things.

-}

data PlayerAction
    = Go String
    | Back
    | Inventory
    | Description
    | Inspect String
    | Take String
    | Drop String
    | Quit
    | Invalid
    deriving (Show)

parseChoice :: String -> PlayerAction
parseChoice choice
    | firstWord == "go" = if secondWord == "back"
        then
            Back
        else
            Go firstRest
    | firstWord == "inventory" = Inventory
    | oneOf firstWord ["description", "room", "where"] = Description
    | firstWord == "inspect" = Inspect firstRest
    | firstWord == "take" = Take firstRest
    | firstWord == "drop" = Drop firstRest
    | firstWord == "quit" = Quit
    | otherwise = Invalid
    where
        firstWord  = toLower $ head $ words choice
        firstRest  = unwords $ drop 1 $ words choice
        secondWord = toLower $ head $ tail $ words choice

oneOf :: Eq a => a -> [a] -> Bool
oneOf x xs = (not . null) $ filter (x ==) xs

toLower :: String -> String
toLower = map Data.Char.toLower

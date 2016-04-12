module HaskQuest.Parser
    (
        PlayerAction (..),
        parseChoice
    )
    where

import qualified Data.Char (toLower)

{-
For handling user input and then doing things.

-}

data PlayerAction
    = Go String
    | Back
    deriving (Show)

parseChoice :: String -> PlayerAction
parseChoice choice
    | firstWord == "go" = if secondWord == "back" then
            Back
        else
            Go firstRest
    | otherwise = error ("No such action: " ++ choice)
    where
        firstWord = toLower $ head $ words choice
        firstRest = unwords $ drop 1 $ words choice
        secondWord = toLower $ head $ tail $ words choice

toLower :: String -> String
toLower s = map Data.Char.toLower s

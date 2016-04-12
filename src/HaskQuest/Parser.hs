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

data PlayerAction = Go String deriving (Show)

parseChoice :: String -> PlayerAction
parseChoice choice
    | firstWord == "go" = Go rest
    | otherwise = error ("No such action: " ++ choice)
    where
        firstWord = toLower $ head $ words choice
        rest = unwords $ drop 1 $ words choice

toLower :: String -> String
toLower s = map Data.Char.toLower s

module HaskQuest.Game.State.SimpleState where

import Control.Monad

{-
A very simple stateful monad.

Primarily based on the tutorial from:
    https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State

Augmented to better support failure. Successful executions are marked Right,
while failures are marked Left.

`try` allows for execution to be processed step-by-step with error handling.
-}

newtype State s a = State { runState :: s -> Either String (a, s) }

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure x = State (\st -> Right (x, st))
    (<*>)  = ap

instance Monad (State s) where
    return = pure
    prev >>= k = State (
        \st -> case runState prev st of
            Left err
                -- An error of some sort.
                -> Left err
            Right (x, st')
                -- Succesful execution. Proceed!
                -> runState (k x) st')

-- Injets an error into the monad.
stateError :: String -> State s ()
stateError err = State (\st -> Left err)

-- Injects a new state into the monad.
put :: a -> State a ()
put newState = State (\st -> Right ((), newState))

-- Extracts a state in the monad.
get :: State a a
get = State (\st -> Right (st, st))

-- Error handling.
try :: (String -> State s a) -> State s a -> State s a
try handler body = State (
    \st -> case runState body st of
        Left err
            -> runState (handler err) st
        Right (x, st')
            -> Right (x, st'))

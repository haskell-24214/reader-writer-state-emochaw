module Tier2.State where

import Control.Monad.State

data Registers = Registers { ax :: Int, bx :: Int, blink :: Bool, acc :: Int }

emptyRegisters = Registers 0 0 False 0

type Calculation = State Registers Int

plus :: Calculation
plus = do
  (Registers a b blink acc) <- get
  let result = a + b
  put $ Registers a b False result
  return result

minus :: Calculation
minus = do
  (Registers a b blink acc) <- get
  let result = a - b
  put $ Registers a b False result
  return result

productS :: Calculation
productS = do
  (Registers a b blink acc) <- get
  let result = a * b
  put $ Registers a b False result
  return result

divS :: Calculation
divS = do
  (Registers a b blink acc) <- get
  if b == 0
    then do
      put $ Registers 0 0 False 0  
      return 0
    else do
      let result = a `div` b  
      put $ Registers a b False result  
      return result

swap :: Calculation
swap = do
  (Registers a b blink acc) <- get
  put $ Registers b a blink acc
  return acc

blinkS :: Calculation
blinkS = do
  (Registers a b blink acc) <- get
  put $ Registers a b (not blink) acc
  return acc

accS :: Calculation
accS = do
  (Registers a b blink acc) <- get
  let newState = if blink then Registers a b (not blink) acc else Registers acc b (not blink) acc
  put newState
  return acc

number :: Int -> Calculation
number x = do
  (Registers a b blink acc) <- get
  let newState = if blink then Registers a b (not blink) acc else Registers a x (not blink) acc
  put newState
  return x

commandToCalculation :: String -> Calculation
commandToCalculation s =
  case s of
    "+" -> plus
    "-" -> minus
    "*" -> productS
    "swap" -> swap
    "blink" -> blinkS
    "acc" -> accS
    x -> number (read x)

buildCalculation :: [String] -> Calculation
buildCalculation xs = 
  foldl (\a x -> a >>= (\_ -> x)) (state (\s -> (0, s))) (map commandToCalculation xs)

calculate :: [String] -> Int
calculate xs = evalState (buildCalculation xs) emptyRegisters

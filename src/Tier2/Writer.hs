module Tier2.Writer (collectAndSumInOrder) where

import Control.Monad.Writer
import Tier0.Writer (Tree (..))

collectAndSumInOrder :: Num a => Tree a -> Writer (Sum a) [a]
collectAndSumInOrder (Leaf x) = do
  tell (Sum x)  
  return [x]  
collectAndSumInOrder (Branch left x right) = do
  leftValues <- collectAndSumInOrder left  
  tell (Sum x)  
  rightValues <- collectAndSumInOrder right  
  return (leftValues ++ [x] ++ rightValues)  
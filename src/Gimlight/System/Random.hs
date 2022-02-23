module Gimlight.System.Random
    ( choice
    , choiceST
    , randomRST
    , randomST
    ) where

import           Control.Monad.State (MonadState (get, put), State)
import           System.Random       (Random (random, randomR), RandomGen)

choice :: (RandomGen g) => g -> [a] -> (a, g)
choice g xs = (val, g')
  where
    val = xs !! n
    (n, g') = randomR (0, length xs - 1) g

choiceST :: (RandomGen g) => [a] -> State g a
choiceST = stFunc . flip choice

randomRST :: (Random a, RandomGen g) => (a, a) -> State g a
randomRST = stFunc . randomR

randomST :: (RandomGen g, Random a) => State g a
randomST = stFunc random

stFunc :: (RandomGen g) => (g -> (a, g)) -> State g a
stFunc f = do
    g <- get
    let (v, g') = f g
    put g'
    return v

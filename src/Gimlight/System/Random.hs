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
choiceST xs = do
    g <- get
    let (v, g') = choice g xs
    put g'
    return v

randomRST :: (Random a, RandomGen g) => (a, a) -> State g a
randomRST r = do
    g <- get
    let (v, g') = randomR r g
    put g'
    return v

randomST :: (RandomGen g, Random a) => State g a
randomST = do
    g <- get
    let (v, g') = random g
    put g'
    return v

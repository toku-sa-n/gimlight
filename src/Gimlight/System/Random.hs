module Gimlight.System.Random
    ( choice
    , choiceST
    ) where

import           Control.Monad.State (MonadState (get, put), State)
import           System.Random       (Random (randomR), RandomGen)

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

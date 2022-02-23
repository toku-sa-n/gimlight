module Gimlight.System.Random
    ( choice
    ) where

import           System.Random (Random (randomR), RandomGen)

choice :: (RandomGen g) => g -> [a] -> (a, g)
choice g xs = (val, g')
  where
    val = xs !! n
    (n, g') = randomR (0, length xs - 1) g

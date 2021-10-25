module Dungeon.Stairs
    ( StairsPair(..)
    ) where

import           Coord (Coord)

data StairsPair = StairsPair
                { upStairs   :: Coord
                , downStairs :: Coord
                }

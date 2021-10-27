module Dungeon.Predefined.BatsCave
    ( batsDungeon
    ) where

import           Coord            (Coord)
import           Dungeon          (Dungeon)
import           Dungeon.Generate (generateMultipleFloorsDungeon)
import           Linear.V2        (V2 (V2))
import           System.Random    (StdGen)
import           TreeZipper       (TreeZipper)

batsDungeon :: StdGen -> (Coord, TreeZipper Dungeon)
batsDungeon g = (pos, d)
    where (d, pos, _) = generateMultipleFloorsDungeon g 3 10 5 8 (V2 50 50)

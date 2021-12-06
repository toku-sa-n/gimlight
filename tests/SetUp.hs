module SetUp
    ( initCellMap
    , initTileCollection
    , playerPosition
    ) where

import           Actor            (player)
import           Coord            (Coord)
import           Data.Array       (array)
import           Data.Maybe       (fromJust)
import           Dungeon.Map.Cell (CellMap, TileIdLayer (TileIdLayer), cellMap,
                                   locateActorAt, locateItemAt)
import           Dungeon.Map.Tile (TileCollection, tile)
import           IndexGenerator   (generator)
import           Item             (herb)
import           Linear.V2        (V2 (V2))

initCellMap :: CellMap
initCellMap =
    fromJust $
    locateActorAt
        p
        playerPosition
        (cellMap $
         array (V2 0 0, V2 1 0) [(V2 0 0, emptyTile), (V2 1 0, emptyTile)]) >>=
    locateItemAt herb playerPosition
  where
    p = fst $ player generator
    emptyTile = TileIdLayer Nothing Nothing

initTileCollection :: TileCollection
initTileCollection = array (0, 0) [(0, tile True True)]

playerPosition :: Coord
playerPosition = V2 0 0

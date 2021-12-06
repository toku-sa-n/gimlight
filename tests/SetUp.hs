module SetUp
    ( initCellMap
    , initTileCollection
    ) where

import           Data.Array       (array)
import           Dungeon.Map.Cell (CellMap, TileIdLayer (TileIdLayer), cellMap)
import           Dungeon.Map.Tile (TileCollection, tile)
import           Linear.V2        (V2 (V2))

initCellMap :: CellMap
initCellMap =
    cellMap $ array (V2 0 0, V2 0 0) [(V2 0 0, TileIdLayer Nothing Nothing)]

initTileCollection :: TileCollection
initTileCollection = array (0, 0) [(0, tile True True)]

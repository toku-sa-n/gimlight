module SetUp
    ( initCellMap
    ) where

import           Data.Array       (array)
import           Dungeon.Map.Cell (CellMap, TileIdLayer (TileIdLayer), cellMap)
import           Linear.V2        (V2 (V2))

initCellMap :: CellMap
initCellMap =
    cellMap $ array (V2 0 0, V2 0 0) [(V2 0 0, TileIdLayer Nothing Nothing)]

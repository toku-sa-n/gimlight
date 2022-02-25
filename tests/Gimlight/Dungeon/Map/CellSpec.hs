module Gimlight.Dungeon.Map.CellSpec
    ( emptyTile
    , emptyCellMap
    ) where

import           Data.Array                (listArray)
import           Gimlight.Dungeon.Map.Cell (CellMap, TileIdLayer (TileIdLayer),
                                            cellMap)
import           Linear                    (V2 (V2))

emptyTile :: TileIdLayer
emptyTile = TileIdLayer Nothing Nothing

emptyCellMap :: V2 Int -> CellMap
emptyCellMap widthHeight =
    cellMap $ listArray (V2 0 0, widthHeight - V2 1 1) $ repeat emptyTile

module Gimlight.Dungeon.Map.CellSpec
    ( emptyTile
    ) where

import           Gimlight.Dungeon.Map.Cell (TileIdLayer (TileIdLayer))

emptyTile :: TileIdLayer
emptyTile = TileIdLayer Nothing Nothing

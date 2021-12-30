module Dungeon.Predefined.GlobalMap
    ( globalMap
    ) where

import           Dungeon                (Dungeon, dungeon)
import           Dungeon.Identifier     (Identifier (GlobalMap))
import           Dungeon.Map.JSONReader (readMapTileImage)
import           Dungeon.Map.Tile       (TileCollection)

globalMap :: TileCollection -> IO (Dungeon, TileCollection)
globalMap tc = do
    (cm, tc') <- readMapTileImage tc "maps/global_map.json"
    return (dungeon cm GlobalMap, tc')

module Dungeon.Predefined.GlobalMap
    ( globalMap
    ) where

import           Dungeon                (Dungeon, dungeon)
import           Dungeon.Identifier     (Identifier (GlobalMap))
import           Dungeon.Map.JSONReader (readMapTileImage)
import           Dungeon.Map.Tile       (TileCollection)
import           UI.Graphics.MapTiles   (MapTiles)

globalMap ::
       TileCollection -> MapTiles -> IO (Dungeon, TileCollection, MapTiles)
globalMap tc mt = do
    (cm, tc', mt') <- readMapTileImage tc mt "maps/global_map.json"
    return (dungeon cm GlobalMap, tc', mt')

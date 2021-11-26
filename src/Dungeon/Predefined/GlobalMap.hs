module Dungeon.Predefined.GlobalMap
    ( globalMap
    ) where

import           Data.Maybe             (fromMaybe)
import           Dungeon                (Dungeon, dungeon)
import           Dungeon.Identifier     (Identifier (GlobalMap))
import qualified Dungeon.Map.JSONReader as JSONReader
import           Dungeon.Map.Tile       (CellMap)

globalMap :: IO Dungeon
globalMap = do
    tileMap <- readMapFile
    return $ dungeon tileMap [] [] GlobalMap

readMapFile :: IO CellMap
readMapFile = do
    tileMap <- JSONReader.readMapFile "maps/global_map.json"
    return $
        fromMaybe
            (error "Failed to read the map file of the global map.")
            tileMap

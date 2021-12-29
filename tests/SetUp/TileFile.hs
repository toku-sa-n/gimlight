module SetUp.TileFile
    ( tilesInSingleTileFile
    , singleTileFile
    ) where

import           Data.Map         (fromList)
import           Dungeon.Map.Tile (TileCollection, tile)

tilesInSingleTileFile :: TileCollection
tilesInSingleTileFile = fromList [((singleTileFile, 0), tile True True)]

singleTileFile :: FilePath
singleTileFile = "tests/tiles/single.json"

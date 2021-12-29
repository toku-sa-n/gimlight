module SetUp.TileFile
    ( tilesInSingleTileFile
    , unitedTileFile
    , singleTileFile
    ) where

import           Data.Map         (fromList)
import           Dungeon.Map.Tile (TileCollection, tile)

tilesInSingleTileFile :: TileCollection
tilesInSingleTileFile = fromList [((singleTileFile, 0), tile True True)]

unitedTileFile :: FilePath
unitedTileFile = "tests/tiles/united.json"

singleTileFile :: FilePath
singleTileFile = "tests/tiles/single.json"

module SetUp.TileFile
    ( tilesInUnitedTileFile
    , tilesInSingleTileFile
    , unitedTileFile
    , singleTileFile
    ) where

import           Data.Map         (fromList)
import           Dungeon.Map.Tile (TileCollection, tile)

tilesInUnitedTileFile :: TileCollection
tilesInUnitedTileFile =
    fromList $ fmap (\x -> ((unitedTileFile, x), tileOfIndex x)) [0 .. 5]
  where
    tileOfIndex n
        | n == unwalkableAndUntransparentTile = tile False False
        | otherwise = tile True True
    unwalkableAndUntransparentTile = 2

tilesInSingleTileFile :: TileCollection
tilesInSingleTileFile = fromList [((singleTileFile, 0), tile True True)]

unitedTileFile :: FilePath
unitedTileFile = "tests/tiles/united.json"

singleTileFile :: FilePath
singleTileFile = "tests/tiles/single.json"

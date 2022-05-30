module Gimlight.SetUp.TileFile
    ( tilesInUnitedTileFile
    , tilesInSingleTileFile
    , tilesInSeaTileFile
    , unitedTileFile
    , singleTileFile
    , seaTileFile
    , tileFileForGeneration
    , tileWithoutProperties
    ) where

import           Codec.Picture             (Image, PixelRGBA8)
import           Data.Foldable             (foldlM)
import           Data.Map                  (empty, singleton, union)
import           Gimlight.Dungeon.Map.Tile (Tile, TileCollection, TileId,
                                            TileType (FloorTile, SeaTile, WallTile),
                                            tile)
import           Gimlight.Prelude
import           Gimlight.SetUp.ImageFile  (singleTileImage)

tilesInUnitedTileFile :: IO TileCollection
tilesInUnitedTileFile = foldlM foldStep empty [0 .. 5]
  where
    foldStep acc x =
        fmap
            (union acc .
             uncurry singleton . tileIdAndTile unitedTileFile x (tileOfIndex x))
            (singleTileImage x)
    tileOfIndex n
        | n == unwalkableAndUntransparentTile = tile WallTile
        | otherwise = tile FloorTile
    unwalkableAndUntransparentTile = 2

tilesInSingleTileFile :: IO TileCollection
tilesInSingleTileFile =
    uncurry singleton . tileIdAndTile singleTileFile 0 (tile FloorTile) <$>
    singleTileImage 0

tilesInSeaTileFile :: IO TileCollection
tilesInSeaTileFile =
    uncurry singleton . tileIdAndTile seaTileFile 0 (tile SeaTile) <$>
    singleTileImage 0

unitedTileFile :: FilePath
unitedTileFile = "tests/tiles/united.json"

singleTileFile :: FilePath
singleTileFile = "tests/tiles/single.json"

seaTileFile :: FilePath
seaTileFile = "tests/tiles/sea.json"

tileWithoutProperties :: FilePath
tileWithoutProperties = "tests/tiles/no_properties.json"

tileFileForGeneration :: FilePath
tileFileForGeneration = "tests/tiles/generate.json"

tileIdAndTile ::
       FilePath
    -> Int
    -> (Image PixelRGBA8 -> Tile)
    -> Image PixelRGBA8
    -> (TileId, Tile)
tileIdAndTile path idx tileGen img = ((path, idx), tileGen img)

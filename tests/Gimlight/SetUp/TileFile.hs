module Gimlight.SetUp.TileFile
    ( tilesInUnitedTileFile
    , tilesInSingleTileFile
    , tilesInUnwalkableTileFile
    , unitedTileFile
    , singleTileFile
    , unwalkableTileFile
    , haskellTilePath
    , tileFileForGeneration
    , tileWithoutProperties
    ) where

import           Codec.Picture             (Image, PixelRGBA8)
import           Data.Foldable             (foldlM)
import           Data.Map                  (empty, singleton, union)
import           Gimlight.Dungeon.Map.Tile (Tile, TileCollection, TileId, tile)
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
        | n == unwalkableAndUntransparentTile = tile False False
        | otherwise = tile True True
    unwalkableAndUntransparentTile = 2

tilesInSingleTileFile :: IO TileCollection
tilesInSingleTileFile =
    uncurry singleton . tileIdAndTile singleTileFile 0 (tile True True) <$>
    singleTileImage 0

tilesInUnwalkableTileFile :: IO TileCollection
tilesInUnwalkableTileFile =
    uncurry singleton . tileIdAndTile unwalkableTileFile 0 (tile False True) <$>
    singleTileImage 0

unitedTileFile :: FilePath
unitedTileFile = "tests/tiles/united.json"

singleTileFile :: FilePath
singleTileFile = "tests/tiles/single.json"

unwalkableTileFile :: FilePath
unwalkableTileFile = "tests/tiles/unwalkable.json"

haskellTilePath :: FilePath
haskellTilePath = "tests/tiles/haskell.json"

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

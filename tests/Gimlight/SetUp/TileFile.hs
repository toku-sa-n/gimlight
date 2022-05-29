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
import           Data.Map                  (fromList)
import           Gimlight.Dungeon.Map.Tile (Tile, TileCollection, TileId, tile)
import           Gimlight.SetUp.ImageFile  (singleTileImage)

tilesInUnitedTileFile :: IO TileCollection
tilesInUnitedTileFile = fromList <$> foldlM foldStep [] [0 .. 5]
  where
    foldStep :: [(TileId, Tile)] -> Int -> IO [(TileId, Tile)]
    foldStep acc x =
        fmap
            ((acc ++) . (: []) . tileList unitedTileFile x (tileOfIndex x))
            (singleTileImage x)
    tileOfIndex n
        | n == unwalkableAndUntransparentTile = tile False False
        | otherwise = tile True True
    unwalkableAndUntransparentTile = 2

tilesInSingleTileFile :: IO TileCollection
tilesInSingleTileFile =
    fromList . (: []) . tileList singleTileFile 0 (tile True True) <$>
    singleTileImage 0

tilesInUnwalkableTileFile :: IO TileCollection
tilesInUnwalkableTileFile =
    fromList . (: []) . tileList unwalkableTileFile 0 (tile False True) <$>
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

tileList ::
       FilePath
    -> Int
    -> (Image PixelRGBA8 -> Tile)
    -> Image PixelRGBA8
    -> (TileId, Tile)
tileList path idx tileGen img = ((path, idx), tileGen img)

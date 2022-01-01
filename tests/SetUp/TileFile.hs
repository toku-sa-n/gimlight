{-# LANGUAGE TupleSections #-}

module SetUp.TileFile
    ( tilesInUnitedTileFile
    , tilesInSingleTileFile
    , tilesInUnwalkableTileFile
    , haskellTile
    , unitedTileFile
    , singleTileFile
    , unwalkableTileFile
    ) where

import           Data.Map         (fromList)
import           Dungeon.Map.Tile (TileCollection, tile)
import           SetUp.ImageFile  (haskellTileImage, singleTileImage)

tilesInUnitedTileFile :: IO TileCollection
tilesInUnitedTileFile =
    fromList <$> mapM (\x -> ((unitedTileFile, x), ) <$> tileOfIndex x) [0 .. 5]
  where
    tileOfIndex n
        | n == unwalkableAndUntransparentTile =
            tile False False <$> singleTileImage n
        | otherwise = tile True True <$> singleTileImage n
    unwalkableAndUntransparentTile = 2

tilesInSingleTileFile :: IO TileCollection
tilesInSingleTileFile =
    fmap (fromList . (: [])) ((singleTileFile, 0), ) . tile True True <$>
    singleTileImage 0

tilesInUnwalkableTileFile :: IO TileCollection
tilesInUnwalkableTileFile =
    fmap (fromList . (: [])) ((unwalkableTileFile, 0), ) . tile False True <$>
    singleTileImage 0

haskellTile :: IO TileCollection
haskellTile =
    fmap (fromList . (: [])) ((haskellTilePath, 0), ) . tile True True <$>
    haskellTileImage

unitedTileFile :: FilePath
unitedTileFile = "tests/tiles/united.json"

singleTileFile :: FilePath
singleTileFile = "tests/tiles/single.json"

unwalkableTileFile :: FilePath
unwalkableTileFile = "tests/tiles/unwalkable.json"

haskellTilePath :: FilePath
haskellTilePath = "tests/tiles/haskell.json"

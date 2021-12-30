{-# LANGUAGE TupleSections #-}

module SetUp.TileFile
    ( tilesInUnitedTileFile
    , tilesInSingleTileFile
    , tilesInUnwalkableTileFile
    , unitedTileFile
    , singleTileFile
    , unwalkableTileFile
    ) where

import           Codec.Picture    (Image, PixelRGBA8, convertRGBA8, readImage)
import           Data.Map         (fromList)
import           Dungeon.Map.Tile (TileCollection, tile)

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

unitedTileFile :: FilePath
unitedTileFile = "tests/tiles/united.json"

singleTileFile :: FilePath
singleTileFile = "tests/tiles/single.json"

unwalkableTileFile :: FilePath
unwalkableTileFile = "tests/tiles/unwalkable.json"

singleTileImage :: Int -> IO (Image PixelRGBA8)
singleTileImage =
    fmap (convertRGBA8 . rightOrError) . readImage . singleTileImagePath
  where
    rightOrError (Right x) = x
    rightOrError (Left x)  = error $ "Failed to load an image: " ++ x

singleTileImagePath :: Int -> FilePath
singleTileImagePath n
    | n < numOfSeparatedTileImages = indexToPath
    | otherwise = "No such file: " ++ indexToPath
  where
    indexToPath = "tests/images/tiles/single_" ++ show n ++ ".png"

numOfSeparatedTileImages :: Int
numOfSeparatedTileImages = 6

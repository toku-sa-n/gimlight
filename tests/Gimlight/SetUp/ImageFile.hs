module Gimlight.SetUp.ImageFile
    ( singleTileImage
    , singleTileImagePath
    ) where

import           Codec.Picture          (Image, PixelRGBA8, convertRGBA8)
import           Gimlight.Codec.Picture (readImage)
import           Gimlight.Data.Either   (expectRight)
import           Gimlight.Prelude

singleTileImage :: Int -> IO (Image PixelRGBA8)
singleTileImage = readImageOrError . singleTileImagePath

singleTileImagePath :: Int -> FilePath
singleTileImagePath n
    | n < numOfSeparatedTileImages = indexToPath
    | otherwise = "No such file: " <> indexToPath
  where
    indexToPath = "tests/images/tiles/single_" <> showt n <> ".png"

readImageOrError :: FilePath -> IO (Image PixelRGBA8)
readImageOrError =
    fmap (convertRGBA8 . expectRight "Failed to load an image") . readImage

numOfSeparatedTileImages :: Int
numOfSeparatedTileImages = 6

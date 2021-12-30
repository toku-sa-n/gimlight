module SetUp.ImageFile
    ( singleTileImage
    , separatedTileImagePath
    , unitedTileImageFilePath
    ) where

import           Codec.Picture (Image, PixelRGBA8, convertRGBA8, readImage)

singleTileImage :: Int -> IO (Image PixelRGBA8)
singleTileImage =
    fmap (convertRGBA8 . rightOrError) . readImage . separatedTileImagePath
  where
    rightOrError (Right x) = x
    rightOrError (Left x)  = error $ "Failed to load an image: " ++ x

separatedTileImagePath :: Int -> FilePath
separatedTileImagePath n
    | n < numOfSeparatedTileImages = indexToPath
    | otherwise = "No such file: " ++ indexToPath
  where
    indexToPath = "tests/images/tiles/separated_" ++ show n ++ ".png"

numOfSeparatedTileImages :: Int
numOfSeparatedTileImages = 6

unitedTileImageFilePath :: FilePath
unitedTileImageFilePath = "tests/images/tiles/united.png"

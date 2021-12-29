module SetUp.ImageFile
    ( separatedTileImagePath
    , unitedTileImageFilePath
    ) where

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

module SetUp.ImageFile
    ( separatedTileImage
    ) where

separatedTileImage :: Int -> FilePath
separatedTileImage n
    | n < numOfSeparatedTileImages = indexToPath
    | otherwise = "No such file: " ++ indexToPath
  where
    indexToPath = "tests/images/tiles/separated_" ++ show n ++ ".png"

numOfSeparatedTileImages :: Int
numOfSeparatedTileImages = 6

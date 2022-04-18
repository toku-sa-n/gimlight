module Gimlight.Actor.WalkingImages
    ( WalkingImages
    , readIntegratedImagesRecursive
    , numOfPatterns
    ) where

import           Codec.Picture              (Image, PixelRGBA8, convertRGBA8,
                                             readImage)
import           Codec.Picture.Extra        (crop)
import           Control.Lens               (Ixed (ix), (^?!))
import           Data.Foldable              (foldlM)
import           Data.Ix                    (Ix (range))
import           Data.Map                   (Map, empty, fromList, union)
import           Gimlight.Coord             (Coord)
import           Gimlight.Data.Either       (expectRight)
import           Gimlight.Direction         (Direction (East, North, NorthEast, NorthWest, South, SouthEast, SouthWest, West),
                                             allDirections)
import           Gimlight.System.Path       (canonicalizeToUnixStyleRelativePath)
import           Gimlight.UI.Draw.Config    (tileHeight, tileWidth)
import           Linear                     (V2 (V2))
import           System.Directory.Recursive (getFilesRecursive)
import           System.FilePath            (takeExtension)

type WalkingImages = Map (FilePath, Direction, Int) (Image PixelRGBA8)

readIntegratedImagesRecursive :: FilePath -> IO WalkingImages
readIntegratedImagesRecursive dir =
    getFilesRecursive dir >>= foldlM addIntegratedImage empty . filterToPng
  where
    filterToPng = filter ((== ".png") . takeExtension)

addIntegratedImage :: WalkingImages -> FilePath -> IO WalkingImages
addIntegratedImage images = fmap (union images) . readAndParseIntegratedImage

readAndParseIntegratedImage :: FilePath -> IO WalkingImages
readAndParseIntegratedImage path =
    readImage path >>= splitImage path . convertRGBA8 . unwrap
  where
    unwrap = expectRight $ "Failed to read an image: " <> path

splitImage :: FilePath -> Image PixelRGBA8 -> IO WalkingImages
splitImage path img = do
    canonicalized <- canonicalizeToUnixStyleRelativePath path
    return $ fromList $ fmap (keyToPair canonicalized) dirAndPatterns
  where
    keyToPair p (dir, pat) = ((p, dir, pat), extractPattern dir pat img)
    dirAndPatterns = (,) <$> allDirections <*> [0 .. numOfPatterns - 1]

extractPattern :: Direction -> Int -> Image PixelRGBA8 -> Image PixelRGBA8
extractPattern dir n = crop leftTopX leftTopY tileWidth tileHeight
  where
    V2 leftTopX leftTopY =
        V2 (tileWidth * n) 0 +
        V2 (tileWidth * numOfPatterns) tileHeight *
        (directionsAndCoords ^?! ix dir)

-- The order of this Direction list must match the order in which actors
-- in a walking image are facing in, from top-left to bottom-left, then
-- top-right, and bottom-right.
directionsAndCoords :: Map Direction Coord
directionsAndCoords =
    fromList $
    zip [South, West, East, North, SouthWest, SouthEast, NorthWest, NorthEast]
        (range (V2 0 0, V2 1 3))

numOfPatterns :: Int
numOfPatterns = 3

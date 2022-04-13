module Gimlight.Actor.WalkingImages
    ( WalkingImages
    , readIntegratedImagesRecursive
    , numOfPatterns
    ) where

import           Codec.Picture              (Image, PixelRGBA8, convertRGBA8,
                                             readImage)
import           Codec.Picture.Extra        (crop)
import           Control.Lens               (Ixed (ix), (^?!))
import           Control.Monad              ((>=>))
import           Data.Foldable              (foldlM)
import           Data.Ix                    (Ix (range))
import           Data.Map                   (Map, empty, fromList, union)
import           Gimlight.Coord             (Coord)
import           Gimlight.Data.Either       (expectRight)
import           Gimlight.Direction         (Direction (East, North, NorthEast, NorthWest, South, SouthEast, SouthWest, West))
import           Gimlight.UI.Draw.Config    (tileHeight, tileWidth)
import           Linear                     (V2 (V2))
import           System.Directory           (canonicalizePath,
                                             makeRelativeToCurrentDirectory)
import           System.Directory.Recursive (getFilesRecursive)
import           System.FilePath            (takeExtension)

type WalkingImages = Map (FilePath, Direction, Int) (Image PixelRGBA8)

readIntegratedImagesRecursive :: FilePath -> IO WalkingImages
readIntegratedImagesRecursive dir =
    getFilesRecursive dir >>=
    foldlM (flip addIntegratedImage) empty . filterToPng
  where
    filterToPng = filter ((== ".png") . takeExtension)

addIntegratedImage :: FilePath -> WalkingImages -> IO WalkingImages
addIntegratedImage path images =
    union images <$> readAndParseIntegratedImage path

readAndParseIntegratedImage :: FilePath -> IO WalkingImages
readAndParseIntegratedImage path = do
    canonicalizedPath <- canonicalizeAsRelative path
    splitImage path . convertRGBA8 . unwrap <$> readImage canonicalizedPath
  where
    unwrap = expectRight $ "Failed to read an image: " <> path
    canonicalizeAsRelative = canonicalizePath >=> makeRelativeToCurrentDirectory

splitImage :: FilePath -> Image PixelRGBA8 -> WalkingImages
splitImage path img = fromList $ fmap keyToPair dirAndPatterns
  where
    keyToPair (dir, pat) = ((path, dir, pat), extractPattern dir pat img)
    dirAndPatterns =
        [(dir, pat) | dir <- [minBound ..], pat <- [0 .. numOfPatterns - 1]]

extractPattern :: Direction -> Int -> Image PixelRGBA8 -> Image PixelRGBA8
extractPattern dir n = crop leftTopX leftTopY tileWidth tileHeight
  where
    V2 leftTopX leftTopY =
        V2 (tileWidth * n) 0 +
        V2 (tileWidth * numOfPatterns) tileHeight *
        (directionsAndCoords ^?! ix dir)

-- This must be the same order as the directions in a walking image.
directionsAndCoords :: Map Direction Coord
directionsAndCoords =
    fromList $
    zip [South, West, East, North, SouthWest, SouthEast, NorthWest, NorthEast]
        (range (V2 0 0, V2 1 3))

numOfPatterns :: Int
numOfPatterns = 3

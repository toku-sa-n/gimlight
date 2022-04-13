{-# LANGUAGE TupleSections #-}

module Gimlight.Actor.WalkingImagesSpec
    ( spec
    ) where

import           Codec.Picture                (Image, PixelRGBA8, convertRGBA8,
                                               readImage)
import           Data.Either.Combinators      (fromRight')
import qualified Data.Map                     as Map
import           Gimlight.Actor.WalkingImages (numOfPatterns,
                                               readIntegratedImagesRecursive)
import           Gimlight.Direction           (Direction)
import           Test.Hspec                   (Spec, describe, it, runIO)

spec :: Spec
spec = describe "readIntegratedImage" testReadIntegratedImageSucceeds

testReadIntegratedImageSucceeds :: Spec
testReadIntegratedImageSucceeds = do
    result <- runIO $ readIntegratedImagesRecursive integratedDir
    expected <-
        runIO $
        Map.fromList <$>
        sequence
            [ ((integrated, d, n), ) <$> readPatternImage d n
            | d <- allPatterns
            , n <- patterns
            ]
    it "reads the specified integrated walking image and splits it to each part" $
        result == expected
  where
    integrated = integratedDir <> "/integrated.png"
    integratedDir = "tests/images/walking/integrated"

readPatternImage :: Direction -> Int -> IO (Image PixelRGBA8)
readPatternImage d =
    fmap (convertRGBA8 . fromRight') . readImage . patternImagePath d

patternImagePath :: Direction -> Int -> FilePath
patternImagePath direction n = directory <> show direction <> show n <> ".png"
  where
    directory = "tests/images/walking/separated/"

patterns :: [Int]
patterns = [0 .. numOfPatterns - 1]

allPatterns :: (Bounded a, Enum a) => [a]
allPatterns = [minBound ..]

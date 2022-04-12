{-# LANGUAGE TupleSections #-}

module Gimlight.Actor.WalkingImagesSpec
    ( spec
    ) where

import           Codec.Picture                (convertRGBA8, readImage)
import           Data.Either.Combinators      (fromRight')
import qualified Data.Map                     as Map
import           Gimlight.Actor.WalkingImages (readIntegratedImagesRecursive)
import           Gimlight.Direction           (Direction (East, North, NorthEast, NorthWest, South, SouthEast, SouthWest, West))
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
            [ ((integrated, d, n), ) . convertRGBA8 . fromRight' <$>
            readImage (parts d n)
            | d <-
                  [ North
                  , South
                  , East
                  , West
                  , NorthEast
                  , NorthWest
                  , SouthEast
                  , SouthWest
                  ]
            , n <- [0 .. 2]
            ]
    it "reads the specified integrated walking image and splits it to each part" $
        result == expected
  where
    integrated = integratedDir <> "/integrated.png"
    integratedDir = "tests/images/walking/integrated"
    parts direction n =
        "tests/images/walking/separated/" <> show direction <> show n <> ".png"

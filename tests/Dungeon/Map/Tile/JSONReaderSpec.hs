{-# LANGUAGE TupleSections #-}

module Dungeon.Map.Tile.JSONReaderSpec
    ( spec
    ) where

import           Control.Arrow               (Arrow (second))
import           Data.Map                    (empty, insert, union)
import           Dungeon.Map.Tile.JSONReader (addTileAndImage, addTileFile)
import           SetUp.ImageFile             (singleTileImage,
                                              singleTileImagePath,
                                              unitedTileImageFilePath)
import           SetUp.TileFile              (singleTileFile,
                                              tilesInSingleTileFile,
                                              tilesInUnitedTileFile,
                                              unitedTileFile)
import           Test.Hspec                  (Spec, describe, it, runIO,
                                              shouldBe)

spec :: Spec
spec = do
    testAddTileFile
    testAddTileAndImage

testAddTileFile :: Spec
testAddTileFile = do
    expected <-
        runIO $
        fmap (, [singleTileImagePath 0, unitedTileImageFilePath]) $
        union <$> tilesInSingleTileFile <*> tilesInUnitedTileFile
    result <-
        runIO $
        addTileFile unitedTileFile empty >>=
        (\(tc, path) -> fmap (second (: [path])) (addTileFile singleTileFile tc))
    describe "addTileFile" $
        it "loads tile information from files and returns the image paths." $
        result `shouldBe` expected

testAddTileAndImage :: Spec
testAddTileAndImage = do
    expectedTiles <-
        runIO $ union <$> tilesInUnitedTileFile <*> tilesInSingleTileFile
    (resultTiles, resultImages) <-
        runIO $
        addTileAndImage unitedTileFile empty empty >>=
        uncurry (addTileAndImage singleTileFile)
    expectedImages <-
        runIO $
        insertMultipleSeparatedFiles [0 .. 5] unitedTileFile empty >>=
        insertMultipleSeparatedFiles [0] singleTileFile
    describe "addTileAndImage" $
        it "loads both a tile file and an image file." $ do
            resultTiles `shouldBe` expectedTiles
            resultImages == expectedImages `shouldBe` True
  where
    insertMultipleSeparatedFiles indexes insertAs m =
        foldl (flip (uncurry insert)) m . zip (zip (repeat insertAs) indexes) <$>
        mapM singleTileImage indexes

{-# LANGUAGE TupleSections #-}

module Dungeon.Map.JSONReaderSpec
    ( spec
    ) where

import           Control.Monad.Except    (runExceptT)
import           Data.Either.Combinators (fromRight')
import           Data.Map                (empty, fromList)
import           Dungeon.Map.JSONReader  (readMapFile, readMapTileImage)
import           SetUp.ImageFile         (singleTileImage)
import           SetUp.MapFile           (cellMapOfSingleTileMap, singleTileMap)
import           SetUp.TileFile          (singleTileFile, tilesInSingleTileFile)
import           Test.Hspec              (Spec, describe, it, runIO, shouldBe)

spec :: Spec
spec = do
    testReadMapFileReturnsTilePath
    testReadMapTileImage

testReadMapFileReturnsTilePath :: Spec
testReadMapFileReturnsTilePath = do
    result <- runIO $ runExceptT $ readMapFile singleTileMap
    describe "readMapFile" .
        it "returns the path to the corresponding tile file." $
        snd (fromRight' result) `shouldBe` singleTileFile

testReadMapTileImage :: Spec
testReadMapTileImage = do
    expectedImage <-
        runIO $
        fromList <$> sequence [((singleTileFile, 0), ) <$> singleTileImage 0]
    (resultCellMap, resultTile, resultImage) <-
        runIO $ readMapTileImage empty empty singleTileMap
    describe "readMapTileImage" $ do
        it "loads the map file" $
            resultCellMap `shouldBe` cellMapOfSingleTileMap
        it "loads the tile file" $ resultTile `shouldBe` tilesInSingleTileFile
        it "loads the image file" $ resultImage == expectedImage `shouldBe` True

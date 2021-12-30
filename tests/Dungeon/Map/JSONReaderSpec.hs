{-# LANGUAGE TupleSections #-}

module Dungeon.Map.JSONReaderSpec
    ( spec
    ) where

import           Control.Monad.Except        (runExceptT)
import           Data.Either.Combinators     (fromRight')
import           Data.Map                    (empty, fromList)
import           Dungeon.Map.JSONReader      (readMapFile, readMapTileImage)
import           Dungeon.Map.Tile.JSONReader (addTileFile)
import           SetUp.ImageFile             (separatedTileImage)
import           SetUp.MapFile               (singleTileMap)
import           SetUp.TileFile              (singleTileFile)
import           Test.Hspec                  (Spec, describe, it, runIO,
                                              shouldBe)

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
    expectedCellMap <-
        fmap (fst . fromRight') . runIO . runExceptT $ readMapFile singleTileMap
    expectedTile <- fmap fst . runIO $ addTileFile singleTileFile empty
    expectedImage <-
        runIO $
        fromList <$> sequence [((singleTileFile, 0), ) <$> separatedTileImage 0]
    (resultCellMap, resultTile, resultImage) <-
        runIO $ readMapTileImage empty empty singleTileMap
    describe "readMapTileImage" $ do
        it "loads the map file" $ resultCellMap `shouldBe` expectedCellMap
        it "loads the tile file" $ resultTile `shouldBe` expectedTile
        it "loads the image file" $ resultImage == expectedImage `shouldBe` True

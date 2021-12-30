module Dungeon.Map.JSONReaderSpec
    ( spec
    ) where

import           Control.Monad.Except    (runExceptT)
import           Data.Either.Combinators (fromRight')
import           Data.Map                (empty)
import           Dungeon.Map.JSONReader  (readMapFile, readMapTileImage)
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
    (resultCellMap, resultTile) <- runIO $ readMapTileImage empty singleTileMap
    expectedTile <- runIO tilesInSingleTileFile
    describe "readMapTileImage" $ do
        it "loads the map file" $
            resultCellMap `shouldBe` cellMapOfSingleTileMap
        it "loads the tile file" $ resultTile `shouldBe` expectedTile

module Dungeon.Map.JSONReaderSpec
    ( spec
    ) where

import           Data.Map               (empty)
import           Dungeon.Map.JSONReader (readMapTileImage)
import           SetUp.MapFile          (cellMapOfSingleTileMap,
                                         rectangleButNotSquareCellMap,
                                         rectangleButNotSquareMap,
                                         singleTileMap)
import           SetUp.TileFile         (tilesInSingleTileFile)
import           Test.Hspec             (Spec, describe, it, runIO, shouldBe)

spec :: Spec
spec = do
    testReadMapTileImage
    testReadRectangleButNotSquareMap

testReadMapTileImage :: Spec
testReadMapTileImage = do
    (resultCellMap, resultTile) <- runIO $ readMapTileImage empty singleTileMap
    expectedTile <- runIO tilesInSingleTileFile
    describe "readMapTileImage" $ do
        it "loads the map file" $
            resultCellMap `shouldBe` cellMapOfSingleTileMap
        it "loads the tile file" $ resultTile `shouldBe` expectedTile

testReadRectangleButNotSquareMap :: Spec
testReadRectangleButNotSquareMap = do
    (resultCellMap, resultTile) <-
        runIO $ readMapTileImage empty rectangleButNotSquareMap
    expectedTile <- runIO tilesInSingleTileFile
    describe "readMapTileImage" $ do
        it "loads the map file" $
            resultCellMap `shouldBe` rectangleButNotSquareCellMap
        it "loads the tile file" $ resultTile `shouldBe` expectedTile

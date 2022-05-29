module Gimlight.Dungeon.Map.JSONReaderSpec
    ( spec
    ) where

import           Gimlight.Dungeon.Map.Cell       (CellMap)
import           Gimlight.Dungeon.Map.JSONReader (readMapFile)
import           Gimlight.SetUp.MapFile          (cellMapContainingMultipleFilesTile,
                                                  cellMapOfSingleTileMap,
                                                  mapUsingMultipleTileFiles,
                                                  mapUsingRotatedTiles,
                                                  rectangleButNotSquareCellMap,
                                                  rectangleButNotSquareMap,
                                                  singleTileMap)
import           Test.Hspec                      (Spec, context, describe,
                                                  errorCall, it, runIO,
                                                  shouldBe, shouldThrow)

spec :: Spec
spec =
    describe "readMapTileImage" $ do
        testSingleTileMap
        testReadRectangleButNotSquareMap
        testReadMapUsingMultipleTileFiles
        testReadMapUsingRotatedTiles

testSingleTileMap :: Spec
testSingleTileMap =
    context "Single tile map" $
    testReadMapFile singleTileMap cellMapOfSingleTileMap

testReadRectangleButNotSquareMap :: Spec
testReadRectangleButNotSquareMap =
    context "Not square map" $
    testReadMapFile rectangleButNotSquareMap rectangleButNotSquareCellMap

testReadMapUsingMultipleTileFiles :: Spec
testReadMapUsingMultipleTileFiles =
    context "Map using multiple tile files." $
    testReadMapFile mapUsingMultipleTileFiles cellMapContainingMultipleFilesTile

testReadMapFile :: FilePath -> CellMap -> Spec
testReadMapFile path cm = do
    resultCellMap <- runIO $ readMapFile path
    it "loads the map file" $ resultCellMap `shouldBe` cm

testReadMapUsingRotatedTiles :: Spec
testReadMapUsingRotatedTiles =
    context "Map using rotated tiles" $
    it "fails to load a map." $
    readMapFile mapUsingRotatedTiles `shouldThrow`
    errorCall
        (mapUsingRotatedTiles ++
         " contains rotated tiles. This game does not support them.")

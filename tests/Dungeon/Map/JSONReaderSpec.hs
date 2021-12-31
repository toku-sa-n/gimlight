module Dungeon.Map.JSONReaderSpec
    ( spec
    ) where

import           Data.Map               (empty, union)
import           Dungeon.Map.Cell       (CellMap)
import           Dungeon.Map.JSONReader (readMapTileImage)
import           Dungeon.Map.Tile       (TileCollection)
import           SetUp.MapFile          (cellMapContainingMultipleFilesTile,
                                         cellMapOfSingleTileMap,
                                         mapUsingMultipleTileFiles,
                                         rectangleButNotSquareCellMap,
                                         rectangleButNotSquareMap,
                                         singleTileMap)
import           SetUp.TileFile         (tilesInSingleTileFile,
                                         tilesInUnitedTileFile)
import           Test.Hspec             (Spec, describe, it, runIO, shouldBe)

spec :: Spec
spec = do
    testSingleTileMap
    testReadRectangleButNotSquareMap
    testReadMapUsingMultipleTileFiles

testSingleTileMap :: Spec
testSingleTileMap =
    runIO tilesInSingleTileFile >>=
    testReadMapTileImage singleTileMap cellMapOfSingleTileMap

testReadRectangleButNotSquareMap :: Spec
testReadRectangleButNotSquareMap =
    runIO tilesInSingleTileFile >>=
    testReadMapTileImage rectangleButNotSquareMap rectangleButNotSquareCellMap

testReadMapUsingMultipleTileFiles :: Spec
testReadMapUsingMultipleTileFiles =
    runIO (union <$> tilesInSingleTileFile <*> tilesInUnitedTileFile) >>=
    testReadMapTileImage
        mapUsingMultipleTileFiles
        cellMapContainingMultipleFilesTile

testReadMapTileImage :: FilePath -> CellMap -> TileCollection -> Spec
testReadMapTileImage path cm tc = do
    (resultCellMap, resultTile) <- runIO $ readMapTileImage empty path
    describe "readMapTileImage" $ do
        it "loads the map file" $ resultCellMap `shouldBe` cm
        it "loads the tile file" $ resultTile `shouldBe` tc

module Gimlight.Dungeon.Map.Tile.JSONReaderSpec
    ( spec
    ) where

import           Control.Monad.IO.Class               (liftIO)
import           Data.Map                             (empty, unions)
import           Gimlight.Dungeon.Map.Tile.JSONReader (addTileFile,
                                                       readTileFileRecursive)
import           Gimlight.SetUp.TileFile              (generateTile,
                                                       haskellTile,
                                                       tileWithoutProperties,
                                                       tilesInSingleTileFile,
                                                       tilesInUnitedTileFile,
                                                       tilesInUnwalkableTileFile)
import           Test.Hspec                           (Spec, describe,
                                                       errorCall, it, runIO,
                                                       shouldBe, shouldThrow)

spec :: Spec
spec = do
    testAddTileFile
    testReadTileFilesRecursive
    testErrorOnReadingTileWithoutProperties

testAddTileFile :: Spec
testAddTileFile =
    describe "addTileFile" $
    it "reads the tile file specified by an argument and add it to the given tile collection." $ do
        expected <- liftIO tilesInUnitedTileFile
        result <- liftIO $ addTileFile "tests/tiles/valid/united.json" empty
        result `shouldBe` expected

testReadTileFilesRecursive :: Spec
testReadTileFilesRecursive = do
    expected <- runIO $ unions <$> sequence tiles
    result <- runIO $ readTileFileRecursive "tests/tiles/valid/"
    describe "readTileFilesRecursive" $
        it "reads all tile files in a directory recursively." $
        result `shouldBe` expected
  where
    tiles =
        [ tilesInUnitedTileFile
        , tilesInSingleTileFile
        , tilesInUnwalkableTileFile
        , haskellTile
        , generateTile
        ]

testErrorOnReadingTileWithoutProperties :: Spec
testErrorOnReadingTileWithoutProperties =
    describe "addTileFile" $
    it "panics if it tries to read a tile that misses necessary proeprties." $
    readTileFileRecursive "tests/tiles/invalid/" `shouldThrow`
    errorCall
        (tileWithoutProperties ++ ": Some tiles miss necessary properties.")

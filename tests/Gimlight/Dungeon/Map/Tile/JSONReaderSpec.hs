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
    it "reads the tile file specified by an argument and add it to the given tile collection." $
    mapM_ testFunc testFileAndExpected
  where
    testFunc (path, expected) = do
        result <- liftIO $ addTileFile path empty
        e <- liftIO expected
        result `shouldBe` e
    testFileAndExpected =
        [ ("tests/tiles/valid/united.json", tilesInUnitedTileFile)
        , ("tests/tiles/valid/single.json", tilesInSingleTileFile)
        , ("tests/tiles/valid/unwalkable.json", tilesInUnwalkableTileFile)
        , ("tests/tiles/valid/haskell.json", haskellTile)
        , ("tests/tiles/valid/generate.json", generateTile)
        ]

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
    addTileFile "tests/tiles/invalid/no_properties.json" empty `shouldThrow`
    errorCall
        (tileWithoutProperties ++ ": Some tiles miss necessary properties.")

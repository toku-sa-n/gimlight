module Gimlight.Dungeon.Map.Tile.JSONReaderSpec
    ( spec
    ) where

import           Control.Monad.IO.Class               (liftIO)
import           Data.Map                             (empty)
import           Gimlight.Dungeon.Map.Tile.JSONReader (addTileFile)
import           Gimlight.SetUp.TileFile              (haskellTile,
                                                       tileWithoutProperties,
                                                       tilesInSingleTileFile,
                                                       tilesInUnitedTileFile,
                                                       tilesInUnwalkableTileFile)
import           Test.Hspec                           (Spec, describe,
                                                       errorCall, it, shouldBe,
                                                       shouldThrow)

spec :: Spec
spec = do
    testAddTileFile
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
        [ ("tests/tiles/united.json", tilesInUnitedTileFile)
        , ("tests/tiles/single.json", tilesInSingleTileFile)
        , ("tests/tiles/unwalkable.json", tilesInUnwalkableTileFile)
        , ("tests/tiles/haskell.json", haskellTile)
        ]

testErrorOnReadingTileWithoutProperties :: Spec
testErrorOnReadingTileWithoutProperties =
    describe "addTileFile" $
    it "panics if it tries to read a tile that misses necessary proeprties." $
    addTileFile "tests/tiles/no_properties.json" empty `shouldThrow`
    errorCall
        (tileWithoutProperties ++ ": Some tiles miss necessary properties.")

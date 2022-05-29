module Gimlight.Dungeon.Map.Tile.JSONReaderSpec
    ( spec
    ) where

import           Control.Monad.IO.Class               (liftIO)
import           Data.Map                             (empty)
import           Data.Text                            (unpack)
import           Gimlight.Dungeon.Map.Tile.JSONReader (addTileFile)
import           Gimlight.Prelude
import           Gimlight.SetUp.TileFile              (tileWithoutProperties,
                                                       tilesInSingleTileFile,
                                                       tilesInUnitedTileFile,
                                                       tilesInUnwalkableTileFile)
import           Test.Hspec                           (Spec, describe,
                                                       errorCall, it, shouldBe,
                                                       shouldThrow)

spec :: Spec
spec =
    describe "addTileFile" $ do
        testAddTileFile
        testErrorOnReadingTileWithoutProperties

testAddTileFile :: Spec
testAddTileFile =
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
        ]

testErrorOnReadingTileWithoutProperties :: Spec
testErrorOnReadingTileWithoutProperties =
    it "panics if it tries to read a tile that misses necessary proeprties." $
    addTileFile "tests/tiles/no_properties.json" empty `shouldThrow`
    errorCall
        (unpack $
         tileWithoutProperties <> ": Some tiles miss necessary properties.")

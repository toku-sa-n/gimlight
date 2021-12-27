module Dungeon.Map.Tile.JSONReaderSpec
    ( spec
    ) where

import           Data.Map                    (empty, fromList)
import           Dungeon.Map.Tile            (tile)
import           Dungeon.Map.Tile.JSONReader (addTileFile)
import           Test.Hspec                  (Spec, describe, it, runIO,
                                              shouldBe)

spec :: Spec
spec = do
    testAddTileFile
    testAddTileFileReturnsImagePath

testAddTileFile :: Spec
testAddTileFile = do
    result <-
        fmap fst . runIO $
        addTileFile unitedTileFile empty >>= addTileFile separatedFile . fst
    describe "addTileFile" $
        it "loads tile information from files." $ result `shouldBe` expected
  where
    expected = fromList $ ((separatedFile, 0), tileOfIndex 0) : unitedList
    unitedList =
        zip
            (zip (repeat unitedTileFile) [0 ..])
            (map tileOfIndex [0 .. tilesInUnited - 1])
    unitedTileFile = "tests/tiles/united.json"
    separatedFile = "tests/tiles/separated_0.json"
    tileOfIndex n
        | n == unwalkableAndUntransparentTile = tile False False
        | otherwise = tile True True
    unwalkableAndUntransparentTile = 2
    tilesInUnited = 6 :: Int

testAddTileFileReturnsImagePath :: Spec
testAddTileFileReturnsImagePath = do
    result <- runIO $ addTileFile "maps/tiles.json" empty
    describe "readTileFile" $
        it "returns the path to the corresponding image file." $
        snd result `shouldBe` expected
  where
    expected = "images/map_tiles.png"

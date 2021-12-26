module Dungeon.Map.Tile.JSONReaderSpec
    ( spec
    ) where

import           Data.Maybe                  (fromJust)
import           Dungeon.Map.Tile.JSONReader (readTileFile)
import           Test.Hspec                  (Spec, describe, it, runIO,
                                              shouldBe)

spec :: Spec
spec = testReadTileFileReturnsImagePath

testReadTileFileReturnsImagePath :: Spec
testReadTileFileReturnsImagePath = do
    result <- runIO $ readTileFile "maps/tiles.json"
    describe "readTileFile" $
        it "returns the path to the corresponding image file." $
        snd (fromJust result) `shouldBe` expected
  where
    expected = "images/map_tiles.png"

module Dungeon.Map.Tile.JSONReaderSpec
    ( spec
    ) where

import           Data.Text                   (pack)
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
        fmap snd result `shouldBe` expected
  where
    expected = Just $ pack "images/map_tiles.png"

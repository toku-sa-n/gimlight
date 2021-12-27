module Dungeon.Map.JSONReaderSpec
    ( spec
    ) where

import           Control.Monad.Trans.Maybe   (MaybeT (runMaybeT))
import           Data.Map                    (empty)
import           Data.Maybe                  (fromJust)
import           Dungeon.Map.JSONReader      (readMapFile, readMapTileImage)
import           Dungeon.Map.Tile.JSONReader (addTileFile)
import           Test.Hspec                  (Spec, describe, it, runIO,
                                              shouldBe)
import qualified UI.Graphics.MapTiles        as MapTiles

spec :: Spec
spec = do
    testReadMapFileReturnsTilePath
    testReadMapTileImage

testReadMapFileReturnsTilePath :: Spec
testReadMapFileReturnsTilePath = do
    result <- runIO $ readMapFile "tests/maps/single_tile.json"
    describe "readMapFile" .
        it "returns the path to the corresponding tile file." $
        snd (fromJust result) `shouldBe` "tests/tiles/separated_0.json"

testReadMapTileImage :: Spec
testReadMapTileImage = do
    expectedCellMap <-
        fmap (fst . fromJust) . runIO $
        readMapFile "tests/maps/single_tile.json"
    expectedTile <-
        fmap fst . runIO $ addTileFile "tests/tiles/separated_0.json" empty
    expectedImage <-
        fmap fromJust . runIO $
        MapTiles.addTileFile
            "tests/tiles/separated_0.json"
            "tests/images/tiles/separated_0.png"
            empty
    (resultCellMap, resultTile, resultImage) <-
        fmap fromJust . runIO $
        runMaybeT $ readMapTileImage empty empty "tests/maps/single_tile.json"
    describe "readMapTileImage" $ do
        it "loads the map file" $ resultCellMap `shouldBe` expectedCellMap
        it "loads the tile file" $ resultTile `shouldBe` expectedTile
        it "loads the image file" $ resultImage == expectedImage `shouldBe` True

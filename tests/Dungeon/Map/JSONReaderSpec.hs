module Dungeon.Map.JSONReaderSpec
    ( spec
    ) where

import           Control.Monad.Except        (runExceptT)
import           Data.Either.Combinators     (fromRight')
import           Data.Map                    (empty)
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
    result <- runIO $ runExceptT $ readMapFile singleTileMap
    describe "readMapFile" .
        it "returns the path to the corresponding tile file." $
        snd (fromRight' result) `shouldBe` separatedTileFile

testReadMapTileImage :: Spec
testReadMapTileImage = do
    expectedCellMap <-
        fmap (fst . fromRight') . runIO . runExceptT $ readMapFile singleTileMap
    expectedTile <- fmap fst . runIO $ addTileFile separatedTileFile empty
    expectedImage <-
        fmap fromRight' . runIO . runExceptT $
        MapTiles.addTileFile
            separatedTileFile
            "tests/images/tiles/separated_0.png"
            empty
    (resultCellMap, resultTile, resultImage) <-
        runIO $ readMapTileImage empty empty singleTileMap
    describe "readMapTileImage" $ do
        it "loads the map file" $ resultCellMap `shouldBe` expectedCellMap
        it "loads the tile file" $ resultTile `shouldBe` expectedTile
        it "loads the image file" $ resultImage == expectedImage `shouldBe` True

singleTileMap :: FilePath
singleTileMap = "tests/maps/single_tile.json"

separatedTileFile :: FilePath
separatedTileFile = "tests/tiles/separated_0.json"

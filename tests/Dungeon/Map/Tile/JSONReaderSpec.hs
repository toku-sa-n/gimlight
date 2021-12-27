module Dungeon.Map.Tile.JSONReaderSpec
    ( spec
    ) where

import           Codec.Picture               (convertRGBA8, readImage)
import           Data.Either.Combinators     (fromRight')
import           Data.Map                    (empty, fromList, insert)
import           Data.Maybe                  (fromJust)
import           Dungeon.Map.Tile            (tile)
import           Dungeon.Map.Tile.JSONReader (addTileAndImage, addTileFile)
import           Test.Hspec                  (Spec, describe, it, runIO,
                                              shouldBe)

spec :: Spec
spec = do
    testAddTileFile
    testAddTileFileReturnsImagePath
    testAddTileAndImage

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

testAddTileAndImage :: Spec
testAddTileAndImage = do
    (resultTiles, resultImages) <-
        fmap fromJust . runIO $ do
            Just (tc, mt) <-
                addTileAndImage "tests/tiles/united.json" empty empty
            addTileAndImage (separatedTileFile 0) tc mt
    expectedImages <-
        runIO $
        insertMultipleSeparatedFiles separatedFiles unitedTileFile empty >>=
        insertMultipleSeparatedFiles [separatedFile 0] (separatedTileFile 0)
    describe "addTileAndImage" $
        it "loads both a tile file and an image file." $ do
            resultTiles `shouldBe` expectedTiles
            resultImages == expectedImages `shouldBe` True
  where
    expectedTiles =
        fromList $
        zip
            ((separatedTileFile 0, 0) : zip (repeat unitedTileFile) [0 ..])
            (tileOfIndex 0 : map tileOfIndex [0 .. tilesInUnited - 1])
    insertMultipleSeparatedFiles fileNames insertAs m =
        foldl (\acc (k, v) -> insert k v acc) m . zip (zipWithIndex insertAs) <$>
        mapM readSingleSeparatedFile fileNames
    readSingleSeparatedFile = fmap (convertRGBA8 . fromRight') . readImage
    zipWithIndex name = zip (repeat name) [0 ..]
    tilesInUnited = 6 :: Int
    tileOfIndex n
        | n == unwalkableAndUntransparentTile = tile False False
        | otherwise = tile True True
    unwalkableAndUntransparentTile = 2
    unitedTileFile = "tests/tiles/united.json"
    separatedFiles = fmap separatedFile [0 :: Int .. 5]
    separatedFile :: Int -> FilePath
    separatedFile n = "tests/images/tiles/separated_" ++ show n ++ ".png"
    separatedTileFile :: Int -> FilePath
    separatedTileFile n = "tests/tiles/separated_" ++ show n ++ ".json"

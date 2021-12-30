module Dungeon.Map.Tile.JSONReaderSpec
    ( spec
    ) where

import           Codec.Picture               (convertRGBA8, readImage)
import           Data.Either.Combinators     (fromRight')
import           Data.Map                    (empty, fromList, insert, union)
import           Dungeon.Map.Tile            (tile)
import           Dungeon.Map.Tile.JSONReader (addTileAndImage, addTileFile)
import           SetUp.ImageFile             (separatedTileImagePath,
                                              unitedTileImageFilePath)
import           SetUp.TileFile              (singleTileFile,
                                              tilesInSingleTileFile,
                                              tilesInUnitedTileFile,
                                              unitedTileFile)
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
        addTileFile unitedTileFile empty >>= addTileFile singleTileFile . fst
    describe "addTileFile" $
        it "loads tile information from files." $ result `shouldBe` expected
  where
    expected = union tilesInSingleTileFile $ fromList unitedList
    unitedList =
        zip
            (zip (repeat unitedTileFile) [0 ..])
            (map tileOfIndex [0 .. tilesInUnited - 1])
    tileOfIndex n
        | n == unwalkableAndUntransparentTile = tile False False
        | otherwise = tile True True
    unwalkableAndUntransparentTile = 2
    tilesInUnited = 6 :: Int

testAddTileFileReturnsImagePath :: Spec
testAddTileFileReturnsImagePath = do
    result <- runIO $ addTileFile unitedTileFile empty
    describe "readTileFile" $
        it "returns the path to the corresponding image file." $
        snd result `shouldBe` unitedTileImageFilePath

testAddTileAndImage :: Spec
testAddTileAndImage = do
    (resultTiles, resultImages) <-
        runIO $ do
            (tc, mt) <- addTileAndImage unitedTileFile empty empty
            addTileAndImage singleTileFile tc mt
    expectedImages <-
        runIO $
        insertMultipleSeparatedFiles separatedFiles unitedTileFile empty >>=
        insertMultipleSeparatedFiles [separatedTileImagePath 0] singleTileFile
    describe "addTileAndImage" $
        it "loads both a tile file and an image file." $ do
            resultTiles `shouldBe` expectedTiles
            resultImages == expectedImages `shouldBe` True
  where
    expectedTiles = tilesInUnitedTileFile `union` tilesInSingleTileFile
    insertMultipleSeparatedFiles fileNames insertAs m =
        foldl (\acc (k, v) -> insert k v acc) m . zip (zipWithIndex insertAs) <$>
        mapM readSingleSeparatedFile fileNames
    readSingleSeparatedFile = fmap (convertRGBA8 . fromRight') . readImage
    zipWithIndex name = zip (repeat name) [0 ..]
    separatedFiles = fmap separatedTileImagePath [0 :: Int .. 5]

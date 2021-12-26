module UI.Graphics.MapTilesSpec
    ( spec
    ) where

import           Codec.Picture           (convertRGBA8, readImage)
import           Data.Either.Combinators (fromRight')
import           Data.Map                (empty, insert)
import           Data.Maybe              (fromJust)
import           System.Directory        (canonicalizePath)
import           Test.Hspec              (Spec, describe, it, runIO, shouldBe)
import           UI.Graphics.MapTiles    (addTileFile)

spec :: Spec
spec = testAddTileFile

testAddTileFile :: Spec
testAddTileFile = do
    tileFile <- runIO $ canonicalizePath unitedImageFile
    result <-
        fmap fromJust . runIO $
        addTileFile tileFile empty >>= addTileFile (separatedFile 0) . fromJust
    expected <-
        runIO $
        insertMultipleSeparatedFiles separatedFiles unitedImageFile empty >>=
        insertMultipleSeparatedFiles [separatedFile 0] (separatedFile 0)
    describe "addTileFile" $
        it
            "separate tile images in the tile file and adds all separated images." $
        -- `Image PixelRGBA8` does not implement `Show`. This is why we do not write `expected `shouldBe` True`.
        result == expected `shouldBe` True
  where
    separatedFiles = fmap separatedFile [0 :: Int .. 5]
    insertMultipleSeparatedFiles fileNames insertAs m =
        foldl (\acc (k, v) -> insert k v acc) m . zip (zipWithIndex insertAs) <$>
        mapM readSingleSeparatedFile fileNames
    zipWithIndex name = zip (repeat name) [0 ..]
    readSingleSeparatedFile = fmap (convertRGBA8 . fromRight') . readImage
    unitedImageFile = "tests/images/tiles/united.png"
    separatedFile :: Int -> FilePath
    separatedFile n = "tests/images/tiles/separated_" ++ show n ++ ".png"

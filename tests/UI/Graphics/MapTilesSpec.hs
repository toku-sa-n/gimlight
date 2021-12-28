module UI.Graphics.MapTilesSpec
    ( spec
    ) where

import           Codec.Picture             (convertRGBA8, readImage)
import           Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import           Data.Either.Combinators   (fromRight')
import           Data.Map                  (empty, insert)
import           Data.Maybe                (fromJust)
import           Test.Hspec                (Spec, describe, it, runIO, shouldBe)
import           UI.Graphics.MapTiles      (addTileFile)

spec :: Spec
spec = testAddTileFile

testAddTileFile :: Spec
testAddTileFile = do
    result <-
        fmap fromJust . runIO . runMaybeT $
        addTileFile dummyUnited unitedImageFile empty >>=
        addTileFile dummySeparated (separatedFile 0)
    expected <-
        runIO $
        insertMultipleSeparatedFiles separatedFiles dummyUnited empty >>=
        insertMultipleSeparatedFiles [separatedFile 0] dummySeparated
    describe "addTileFile" $
        it
            "separate tile images in the tile file and adds all separated images." $
        -- `Image PixelRGBA8` does not implement `Show`. This is why we do not write `expected `shouldBe` True`.
        result == expected `shouldBe` True
  where
    insertMultipleSeparatedFiles fileNames insertAs m =
        foldl (\acc (k, v) -> insert k v acc) m . zip (zipWithIndex insertAs) <$>
        mapM readSingleSeparatedFile fileNames
    zipWithIndex name = zip (repeat name) [0 ..]
    readSingleSeparatedFile = fmap (convertRGBA8 . fromRight') . readImage
    unitedImageFile = "tests/images/tiles/united.png"
    separatedFiles = fmap separatedFile [0 :: Int .. 5]
    separatedFile :: Int -> FilePath
    separatedFile n = "tests/images/tiles/separated_" ++ show n ++ ".png"
    dummyUnited = "united.json"
    dummySeparated = "separated.json"

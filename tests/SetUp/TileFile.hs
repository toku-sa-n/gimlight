{-# LANGUAGE TupleSections #-}

module SetUp.TileFile
    ( tilesInUnitedTileFile
    , tilesInSingleTileFile
    , tilesInUnwalkableTileFile
    , haskellTile
    , unitedTileFile
    , singleTileFile
    , unwalkableTileFile
    , haskellTilePath
    ) where

import           Codec.Picture        (Image (imageData), PixelRGBA8)
import           Codec.Picture.Extra  (flipHorizontally, flipVertically)
import           Data.Bits            (Bits (bit, (.|.)))
import           Data.List            (transpose)
import           Data.List.Split      (chunksOf)
import           Data.Map             (fromList)
import qualified Data.Vector.Storable as V
import           Dungeon.Map.Tile     (Tile, TileCollection, TileIdentifier,
                                       tile)
import           SetUp.ImageFile      (haskellTileImage, singleTileImage)
import           UI.Draw.Config       (tileWidth)

tilesInUnitedTileFile :: IO TileCollection
tilesInUnitedTileFile =
    fromList <$> mapM (\x -> ((unitedTileFile, x), ) <$> tileOfIndex x) [0 .. 5]
  where
    tileOfIndex n
        | n == unwalkableAndUntransparentTile =
            tile False False <$> singleTileImage n
        | otherwise = tile True True <$> singleTileImage n
    unwalkableAndUntransparentTile = 2

tilesInSingleTileFile :: IO TileCollection
tilesInSingleTileFile =
    fmap (fromList . (: [])) ((singleTileFile, 0), ) . tile True True <$>
    singleTileImage 0

tilesInUnwalkableTileFile :: IO TileCollection
tilesInUnwalkableTileFile =
    fmap (fromList . (: [])) ((unwalkableTileFile, 0), ) . tile False True <$>
    singleTileImage 0

haskellTile :: IO TileCollection
haskellTile = fmap (fromList . tileList) haskellTileImage
    -- fmap (fromList . (: [])) ((haskellTilePath, 0), ) . tile True True <$>
    -- haskellTileImage
    -- Transformation order is important. Tiled's specification says
    --
    -- > When rendering an orthographic or isometric tile, the order of
    --   operations matters. The diagonal flip is done first, followed by the
    --   horizontal and vertical flips. The diagonal flip should flip the
    --   bottom left and top right corners of the tile, and can be thought of
    --   as an x/y axis swap. For hexagonal tiles, the order does not matter.
    --
    -- See: https://docs.mapeditor.org/en/stable/reference/global-tile-ids/#gid-tile-flipping
  where
    tileList :: Image PixelRGBA8 -> [(TileIdentifier, Tile)]
    tileList img =
        fmap (`identifierAndTileForDVH` img) diagonalVertialHorizontal
    identifierAndTileForDVH ::
           (Bool, Bool, Bool) -> Image PixelRGBA8 -> (TileIdentifier, Tile)
    identifierAndTileForDVH (d, v, h) img =
        ( (haskellTilePath, tileIdMultiplier d v h 0)
        , tile True True (transformImage d v h img))
    transformImage d v h =
        (if h
             then flipHorizontally
             else id) .
        (if v
             then flipVertically
             else id) .
        (if d
             then swapImageXY
             else id)
    swapImageXY :: Image PixelRGBA8 -> Image PixelRGBA8
    swapImageXY img =
        img
            { imageData =
                  V.fromList
                      (concat $
                       transpose $ chunksOf tileWidth $ V.toList $ imageData img)
            }
    tileIdMultiplier :: Bool -> Bool -> Bool -> Int -> Int
    tileIdMultiplier d v h =
        ((if d
              then bit 29
              else 1) .|.
         (if v
              then bit 30
              else 1) .|.
         (if h
              then bit 31
              else 1) .|.)
    diagonalVertialHorizontal =
        (,,) <$> [False, True] <*> [False, True] <*> [False, True]

unitedTileFile :: FilePath
unitedTileFile = "tests/tiles/united.json"

singleTileFile :: FilePath
singleTileFile = "tests/tiles/single.json"

unwalkableTileFile :: FilePath
unwalkableTileFile = "tests/tiles/unwalkable.json"

haskellTilePath :: FilePath
haskellTilePath = "tests/tiles/haskell.json"

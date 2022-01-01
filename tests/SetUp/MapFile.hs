module SetUp.MapFile
    ( cellMapContainingMultipleFilesTile
    , cellMapOfSingleTileMap
    , rectangleButNotSquareCellMap
    , cellMapUsingRotatedTiles
    , mapUsingMultipleTileFiles
    , singleTileMap
    , rectangleButNotSquareMap
    , mapUsingRotatedTiles
    ) where

import           Codec.Picture    (Image, PixelRGBA8, imagePixels)
import           Control.Lens     ((%~), (&))
import           Data.Array       (array)
import           Data.Bits        (Bits (bit))
import           Dungeon.Map.Cell (CellMap,
                                   TileIdentifierLayer (TileIdentifierLayer),
                                   cellMap)
import           Dungeon.Map.Tile (tile)
import           Linear.V2        (V2 (V2))
import           SetUp.TileFile   (singleTileFile, unitedTileFile)

cellMapContainingMultipleFilesTile :: CellMap
cellMapContainingMultipleFilesTile =
    cellMap $
    array
        (V2 0 0, V2 1 0)
        [ (V2 0 0, TileIdentifierLayer Nothing (Just (singleTileFile, 0)))
        , (V2 1 0, TileIdentifierLayer Nothing (Just (unitedTileFile, 1)))
        ]

cellMapOfSingleTileMap :: CellMap
cellMapOfSingleTileMap =
    cellMap $
    array
        (V2 0 0, V2 0 0)
        [(V2 0 0, TileIdentifierLayer Nothing (Just (singleTileFile, 0)))]

rectangleButNotSquareCellMap :: CellMap
rectangleButNotSquareCellMap =
    cellMap $
    array
        (V2 0 0, V2 1 0)
        [ (V2 x 0, TileIdentifierLayer Nothing (Just (singleTileFile, 0)))
        | x <- [0, 1]
        ]

cellMapUsingRotatedTiles :: CellMap
cellMapUsingRotatedTiles = cellMap $ array (V2 0 0, V2 7 0) $ zip [0 .. 7] tiles
  where
    tiles = [tile True True | (d, v, h) <- diagonalVertialHorizontal]
    swapImageXY :: Image PixelRGBA8 -> Image PixelRGBA8
    swapImageXY img = img & imagePixels %~ (\x -> x)
    -- (x,y)->(y,x)
    -- y*w+x <- x*w+y
    -- tileAfterRotating (d,v,h) = (if
    tileIdMultiplier (d, v, h) =
        ((if d
              then bit 29
              else 1) *
         (if v
              then bit 30
              else 1) *
         (if h
              then bit 31
              else 1) *)
    diagonalVertialHorizontal =
        (,,) <$> [False, True] <*> [False, True] <*> [False, True]

mapUsingMultipleTileFiles :: FilePath
mapUsingMultipleTileFiles = "tests/maps/multiple_tile_files.json"

singleTileMap :: FilePath
singleTileMap = "tests/maps/single_tile.json"

rectangleButNotSquareMap :: FilePath
rectangleButNotSquareMap = "tests/maps/not_square.json"

mapUsingRotatedTiles :: FilePath
mapUsingRotatedTiles = "tests/maps/rotation.json"

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

import           Data.Array       (array)
import           Data.Bits        (Bits (bit, (.|.)))
import           Dungeon.Map.Cell (CellMap,
                                   TileIdentifierLayer (TileIdentifierLayer),
                                   cellMap)
import           Linear.V2        (V2 (V2))
import           SetUp.TileFile   (haskellTilePath, singleTileFile,
                                   unitedTileFile)

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
cellMapUsingRotatedTiles =
    cellMap $
    array (V2 0 0, V2 7 0) $ zipWith (\x t -> (V2 x 0, t)) [0 .. 7] tiles
  where
    tiles =
        [ TileIdentifierLayer
            Nothing
            (Just (haskellTilePath, tileIdMultiplier d v h 0))
        | (d, v, h) <- diagonalVertialHorizontal
        ]
    tileIdMultiplier d v h = (bitIf d 29 .|. bitIf v 30 .|. bitIf h 31 .|.)
    bitIf cond b
        | cond = bit b
        | otherwise = 0
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

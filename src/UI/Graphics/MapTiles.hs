module UI.Graphics.MapTiles
    ( MapTiles
    , addTileFile
    ) where

import           Codec.Picture       (Image (imageHeight, imageWidth),
                                      PixelRGBA8, convertRGBA8, readImage)
import           Codec.Picture.Extra (crop)
import           Control.Monad       (guard)
import           Data.Either         (fromRight)
import           Data.Functor        ((<&>))
import           Data.Map            (Map, insert)
import           UI.Draw.Config      (tileHeight, tileWidth)

type MapTiles = Map (FilePath, Int) (Image PixelRGBA8)

addTileFile :: FilePath -> FilePath -> MapTiles -> IO MapTiles
addTileFile jsonFile path tiles = readTileMapFile path <&> tileFileToMap
  where
    tileFileToMap =
        foldl (\acc (idx, img) -> insert idx img acc) tiles .
        zip (zip (repeat jsonFile) [0 ..]) . cutTileMap

readTileMapFile :: FilePath -> IO (Image PixelRGBA8)
readTileMapFile path = do
    tileFile <- convertRGBA8 . fromRight (error noSuchImage) <$> readImage path
    guard $ isValidTileMapFile tileFile
    return tileFile
  where
    noSuchImage = path ++ " not found."

isValidTileMapFile :: Image PixelRGBA8 -> Bool
isValidTileMapFile img =
    imageWidth img `mod` tileWidth == 0 && imageHeight img `mod` tileHeight == 0

cutTileMap :: Image PixelRGBA8 -> [Image PixelRGBA8]
cutTileMap img =
    [ crop (col * tileWidth) (row * tileHeight) tileWidth tileHeight img
    | row <- [0 .. rowsOfTilesInImage - 1]
    , col <- [0 .. columnsOfTilesInImage - 1]
    ]
  where
    columnsOfTilesInImage = imageWidth img `div` tileWidth
    rowsOfTilesInImage = imageHeight img `div` tileHeight

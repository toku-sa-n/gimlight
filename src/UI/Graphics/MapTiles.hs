module UI.Graphics.MapTiles
    ( MapTiles
    , addTileFile
    ) where

import           Codec.Picture       (Image (imageHeight, imageWidth),
                                      PixelRGBA8, convertRGBA8, readImage)
import           Codec.Picture.Extra (crop)
import           Data.Map            (Map, insert)
import           UI.Draw.Config      (tileHeight, tileWidth)

type MapTiles = Map (FilePath, Int) (Image PixelRGBA8)

addTileFile :: FilePath -> FilePath -> MapTiles -> IO (Maybe MapTiles)
addTileFile jsonFile path tiles = do
    tileFile <- readTileMapFile path
    return $
        foldl (\acc (idx, img) -> insert idx img acc) tiles .
        zip (zip (repeat jsonFile) [0 ..]) . cutTileMap <$>
        tileFile

readTileMapFile :: FilePath -> IO (Maybe (Image PixelRGBA8))
readTileMapFile path = do
    tileFile <- readImage path
    case tileFile of
        Right x -> return $ convertAndCheck x
        Left _  -> error "Failed to read a tile image file."
  where
    convertAndCheck img =
        if isValidTileMapFile rgbaImage
            then Just rgbaImage
            else Nothing
      where
        rgbaImage = convertRGBA8 img

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

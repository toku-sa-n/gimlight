{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Dungeon.Map.TileSpec
    ( mockTileCollection
    , dummyTileFile
    ) where

import           Codec.Picture             (PixelRGBA8 (PixelRGBA8),
                                            generateImage)
import           Data.Map                  (fromList)
import           Gimlight.Dungeon.Map.Tile (TileCollection, tile)
import           Gimlight.UI.Draw.Config   (tileHeight, tileWidth)

mockTileCollection :: TileCollection
mockTileCollection =
    fromList
        [ ((dummyTileFile, 0), tile True True emptyImage)
        , ((dummyTileFile, 1), tile False True emptyImage)
        ]
  where
    emptyImage = generateImage (\_ _ -> PixelRGBA8 0 0 0 0) tileWidth tileHeight

dummyTileFile :: FilePath
dummyTileFile = "dummy.json"

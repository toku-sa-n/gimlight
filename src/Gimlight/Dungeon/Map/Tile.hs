{-# LANGUAGE DeriveGeneric #-}

module Gimlight.Dungeon.Map.Tile
    ( Tile
    , TileType(..)
    , TileCollection
    , TileId
    , TileIndex
    , tile
    , isWalkable
    , isTransparent
    , getImage
    ) where

import           Codec.Picture    (Image (imageData, imageHeight, imageWidth),
                                   PixelRGBA8)
import           Data.Map         (Map)
import           GHC.Generics     (Generic)
import           Gimlight.Prelude

data TileType
    = FloorTile
    | SeaTile
    | WallTile
    deriving (Eq, Ord, Show, Read)

data Tile =
    Tile
        { tileType :: TileType
        , image    :: Image PixelRGBA8
        }
    deriving (Eq, Generic)

instance Show Tile where
    show t = "Tile {tileType = " ++ show (tileType t) ++ ", ...}"

instance Ord Tile where
    a <= b =
        tileType a <= tileType b &&
        imageWidth (image a) <= imageWidth (image b) &&
        imageHeight (image a) <= imageHeight (image b) &&
        imageData (image a) <= imageData (image b)

type TileCollection = Map TileId Tile

type TileId = (FilePath, Int)

type TileIndex = Int

tile :: TileType -> Image PixelRGBA8 -> Tile
tile = Tile

getImage :: Tile -> Image PixelRGBA8
getImage = image

isWalkable :: Tile -> Bool
isWalkable t =
    case tileType t of
        FloorTile -> True
        SeaTile   -> False
        WallTile  -> False

isTransparent :: Tile -> Bool
isTransparent t =
    case tileType t of
        FloorTile -> True
        SeaTile   -> True
        WallTile  -> False

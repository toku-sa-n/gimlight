{-# LANGUAGE DeriveGeneric #-}

module Dungeon.Map.Tile
    ( Tile
    , TileCollection
    , TileIdentifier
    , TileId
    , tile
    , isWalkable
    , isTransparent
    , wallTile
    , floorTile
    , upStairs
    , downStairs
    ) where

import           Data.Binary  (Binary)
import           Data.Map     (Map)
import           GHC.Generics (Generic)

data Tile =
    Tile
        { walkable    :: Bool
        , transparent :: Bool
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary Tile

type TileCollection = Map TileIdentifier Tile

type TileIdentifier = (FilePath, Int)

type TileId = Int

tile :: Bool -> Bool -> Tile
tile = Tile

isWalkable :: Tile -> Bool
isWalkable = walkable

isTransparent :: Tile -> Bool
isTransparent = transparent

floorTile :: TileIdentifier
floorTile = ("maps/tiles.json", 0)

wallTile :: TileIdentifier
wallTile = ("maps/tiles.json", 1)

upStairs :: TileIdentifier
upStairs = ("maps/tiles.json", 3)

downStairs :: TileIdentifier
downStairs = ("maps/tiles.json", 4)

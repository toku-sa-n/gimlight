{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Dungeon.Map.Tile
    ( CellMap
    , cellMap
    , widthAndHeight
    , changeTileAt
    , walkableMap
    , transparentMap
    , tileIdAt
    , isWalkableAt
    , Tile
    , TileCollection
    , TileId
    , tile
    , allWallTiles
    , isWalkable
    , isTransparent
    , floorTile
    , upStairs
    , downStairs
    ) where

import           Actor            (Actor)
import           Control.Lens     (makeLenses, (&), (.~), (^.))
import           Coord            (Coord)
import           Data.Array       (Array, bounds, (!), (//))
import           Data.Binary      (Binary)
import           Data.Maybe       (isJust)
import qualified Dungeon.Map      as M
import           Dungeon.Map.Bool (BoolMap)
import           GHC.Generics     (Generic)
import           Linear.V2        (V2 (V2))

type TileId = Int

data Cell =
    Cell
        { _tileId :: TileId
        , actor   :: Maybe Actor
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''Cell

instance Binary Cell

newtype CellMap =
    CellMap (Array (V2 Int) Cell)
    deriving (Show, Ord, Eq, Generic)

instance Binary CellMap

cellMap :: Array (V2 Int) TileId -> CellMap
cellMap = CellMap . fmap (`Cell` Nothing)

widthAndHeight :: CellMap -> V2 Int
widthAndHeight (CellMap m) = snd (bounds m) + V2 1 1

changeTileAt :: Coord -> TileId -> CellMap -> Maybe CellMap
changeTileAt c i (CellMap m)
    | isJust $ tileIdAt c (CellMap m) = Just $ CellMap $ m // [(c, newTile)]
    | otherwise = Nothing
  where
    newTile = m ! c & tileId .~ i

walkableMap :: TileCollection -> CellMap -> BoolMap
walkableMap tc (CellMap m) = isWalkable . ((tc !) . (^. tileId)) <$> m

transparentMap :: TileCollection -> CellMap -> BoolMap
transparentMap tc (CellMap m) = isTransparent . ((tc !) . (^. tileId)) <$> m

tileIdAt :: Coord -> CellMap -> Maybe TileId
tileIdAt c t = (^. tileId) <$> cellAt c t

cellAt :: Coord -> CellMap -> Maybe Cell
cellAt c (CellMap m)
    | c >= lower && c <= upper = Just $ m ! c
    | otherwise = Nothing
  where
    (lower, upper) = bounds m

isWalkableAt :: Coord -> TileCollection -> CellMap -> Bool
isWalkableAt c tc t =
    case tileIdAt c t of
        Just x  -> isWalkable (tc ! x)
        Nothing -> False

data Tile =
    Tile
        { walkable    :: Bool
        , transparent :: Bool
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary Tile

type TileCollection = Array Int Tile

tile :: Bool -> Bool -> Tile
tile = Tile

allWallTiles :: V2 Int -> CellMap
allWallTiles wh = CellMap $ M.generate wh (const (Cell wallTile Nothing))

isWalkable :: Tile -> Bool
isWalkable = walkable

isTransparent :: Tile -> Bool
isTransparent = transparent

floorTile :: TileId
floorTile = 0

wallTile :: TileId
wallTile = 1

upStairs :: TileId
upStairs = 3

downStairs :: TileId
downStairs = 4

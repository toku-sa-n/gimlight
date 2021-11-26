{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Dungeon.Map.Tile
    ( TileMap
    , tileMap
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

newtype TileMap =
    TileMap (Array (V2 Int) Cell)
    deriving (Show, Ord, Eq, Generic)

instance Binary TileMap

tileMap :: Array (V2 Int) TileId -> TileMap
tileMap = TileMap . fmap (`Cell` Nothing)

widthAndHeight :: TileMap -> V2 Int
widthAndHeight (TileMap m) = snd (bounds m) + V2 1 1

changeTileAt :: Coord -> TileId -> TileMap -> Maybe TileMap
changeTileAt c i (TileMap m)
    | isJust $ tileIdAt c (TileMap m) = Just $ TileMap $ m // [(c, newTile)]
    | otherwise = Nothing
  where
    newTile = m ! c & tileId .~ i

walkableMap :: TileCollection -> TileMap -> BoolMap
walkableMap tc (TileMap m) = isWalkable . ((tc !) . (^. tileId)) <$> m

transparentMap :: TileCollection -> TileMap -> BoolMap
transparentMap tc (TileMap m) = isTransparent . ((tc !) . (^. tileId)) <$> m

tileIdAt :: Coord -> TileMap -> Maybe TileId
tileIdAt c t = (^. tileId) <$> cellAt c t

cellAt :: Coord -> TileMap -> Maybe Cell
cellAt c (TileMap m)
    | c >= lower && c <= upper = Just $ m ! c
    | otherwise = Nothing
  where
    (lower, upper) = bounds m

isWalkableAt :: Coord -> TileCollection -> TileMap -> Bool
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

allWallTiles :: V2 Int -> TileMap
allWallTiles wh = TileMap $ M.generate wh (const (Cell wallTile Nothing))

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

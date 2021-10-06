{-# LANGUAGE TemplateHaskell #-}

module Map.Tile
    ( TileMap
    , allWallTiles
    , Tile
    , wallTile
    , floorTile
    , walkable
    , transparent
    , char
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Array      (Array)
import qualified Map             as M

data Tile = Tile
          { _walkable    :: Bool
          , _transparent :: Bool
          , _char        :: Char
          } deriving (Show, Ord, Eq)
makeLenses ''Tile

type TileMap = Array (Int, Int) Tile

allWallTiles :: TileMap
allWallTiles = M.generate $ const wallTile

wallTile :: Tile
wallTile = Tile { _walkable = False
                , _transparent = False
                , _char = 'X'
                }

floorTile :: Tile
floorTile = Tile { _walkable = True
                 , _transparent = True
                 , _char = '.'
                 }

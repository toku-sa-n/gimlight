-- This module is a workaround to avoid circular dependencies between
-- `Dungeon` and `Entity`. The actual logics are defined in the `Dungeon`
-- and `Entity` modules.
--
-- TODO: Define these types in each module if possible.

{-# LANGUAGE TemplateHaskell #-}

module Dungeon.Types
    ( Dungeon
    , dungeon
    , tileMap
    , visible
    , explored
    , entities
    ) where

import           Control.Lens (makeLenses)
import           Entity       (Entity)
import           Map.Explored (ExploredMap, initExploredMap)
import           Map.Fov      (Fov, initFov)
import           Map.Tile     (TileMap)

data Dungeon = Dungeon
          { _tileMap  :: TileMap
          , _visible  :: Fov
          , _explored :: ExploredMap
          , _entities :: [Entity]
          } deriving (Show)
makeLenses ''Dungeon

dungeon :: TileMap -> [Entity] -> Dungeon
dungeon t e = Dungeon { _tileMap = t
                      , _visible = initFov
                      , _explored = initExploredMap
                      , _entities = e
                      }

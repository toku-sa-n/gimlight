module Dungeon.Predefined
    ( firstEventMap
    ) where

import           Data.Array ((//))
import           Dungeon    (Dungeon, dungeon)
import           Entity     (Entity)
import           Map.Tile   (TileMap, allWallTiles, floorTile, wallTile)

firstEventMap :: Entity -> Dungeon
firstEventMap e = dungeon (stringArrayToMap [ "####################"
                                            , "#..................#"
                                            , "#..................#"
                                            , "#..................#"
                                            , "#..................#"
                                            , "#..................#"
                                            , "#..................#"
                                            , "#..................#"
                                            , "####################"
                                            ])
                [e]

stringArrayToMap :: [String] -> TileMap
stringArrayToMap list = allWallTiles // [((x, y), tile c) | (y, row) <- zip [0..] list, (x, c) <- zip [0..] row]
    where tile c
            | c == '#' = wallTile
            | c == '.' = floorTile
            | otherwise = error "Invalid tile type."

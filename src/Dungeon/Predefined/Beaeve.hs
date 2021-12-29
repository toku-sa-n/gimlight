module Dungeon.Predefined.Beaeve
    ( beaeve
    ) where

import           Actor.Friendly.Electria (electria)
import           Control.Monad.State     (execStateT)
import           Data.Either.Combinators (fromRight)
import           Dungeon                 (Dungeon, dungeon)
import           Dungeon.Identifier      (Identifier (Beaeve))
import           Dungeon.Map.Cell        (locateActorAt)
import           Dungeon.Map.JSONReader  (readMapTileImage)
import           Dungeon.Map.Tile        (TileCollection)
import           IndexGenerator          (IndexGenerator)
import           Linear.V2               (V2 (V2))
import           UI.Graphics.MapTiles    (MapTiles)

beaeve ::
       TileCollection
    -> MapTiles
    -> IndexGenerator
    -> IO (Dungeon, TileCollection, MapTiles, IndexGenerator)
beaeve tc mt ig = do
    (cm, tc', mt') <- readMapTileImage tc mt "maps/beaeve.json"
    let cm' =
            fromRight (error "Failed to place a NPC.") . flip execStateT cm $
            locateActorAt tc' electria' (V2 4 5)
    return (dungeon cm' Beaeve, tc', mt', ig')
  where
    (electria', ig') = electria ig

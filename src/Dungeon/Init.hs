module Dungeon.Init
    ( initDungeon
    ) where

import           Actor                     (player)
import           Control.Lens              ((%%~), (&))
import           Control.Monad.State       (execStateT)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import           Data.Either.Combinators   (rightToMaybe)
import           Dungeon                   (Dungeon, cellMap)
import           Dungeon.Map.Cell          (locateActorAt, updateExploredMap,
                                            updatePlayerFov)
import           Dungeon.Map.Tile          (TileCollection)
import           Dungeon.Predefined.Beaeve (beaeve)
import           IndexGenerator            (IndexGenerator)
import           Linear.V2                 (V2 (V2))
import           UI.Graphics.MapTiles      (MapTiles)

initDungeon ::
       TileCollection
    -> MapTiles
    -> IndexGenerator
    -> MaybeT IO (Dungeon, TileCollection, MapTiles, IndexGenerator)
initDungeon tc mt ig = do
    (beaeve', tc', mt', ig'') <- beaeve tc mt ig'
    d <- MaybeT . return $ beaeve' & cellMap %%~ initBeaeve tc'
    return (d, tc', mt', ig'')
  where
    initBeaeve tc' cm' =
        rightToMaybe (execStateT (locateActorAt tc' player' (V2 5 5)) cm') >>=
        updatePlayerFov tc' >>=
        Just . updateExploredMap
    (player', ig') = player ig

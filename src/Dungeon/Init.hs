module Dungeon.Init
    ( initDungeon
    ) where

import           Actor                     (player)
import           Control.Lens              ((%%~), (&))
import           Control.Monad.Except      (ExceptT (ExceptT))
import           Control.Monad.State       (execStateT)
import           Data.Either.Combinators   (mapLeft, maybeToRight)
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
    -> ExceptT String IO (Dungeon, TileCollection, MapTiles, IndexGenerator)
initDungeon tc mt ig = do
    (beaeve', tc', mt', ig'') <- beaeve tc mt ig'
    d <- ExceptT . return $ beaeve' & cellMap %%~ initBeaeve tc'
    return (d, tc', mt', ig'')
  where
    initBeaeve tc' cm' =
        mapLeft show (execStateT (locateActorAt tc' player' (V2 5 5)) cm') >>=
        maybeToRight "Failed to update the player FoV." . updatePlayerFov tc' >>=
        Right . updateExploredMap
    (player', ig') = player ig

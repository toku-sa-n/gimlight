module Game.Status.Exploring.Dungeons
    ( Dungeons
    , ascendStairsAtPlayerPosition
    ) where

import           Control.Lens   ((%~), (&), (.~), (^.))
import           Dungeon        (Dungeon, actors, ascendingStairs, updateMap)
import qualified Dungeon        as D
import           Dungeon.Actor  (Actor, position)
import           Dungeon.Stairs (StairsPair (downStairs, upStairs))
import           TreeZipper     (TreeZipper, getFocused, goUp, modify)

type Dungeons = TreeZipper Dungeon

ascendStairsAtPlayerPosition :: Dungeons -> Maybe Dungeons
ascendStairsAtPlayerPosition ds = newZipper
    where (player, zipperWithoutPlayer) = popPlayer ds
          newPlayer = case (player, newPosition) of
                          (Just p, Just pos) -> Just $ p & position .~ pos
                          _                  -> Nothing
          ascendable = (downStairs <$> getFocused ds ^. ascendingStairs) == fmap (^. position) player
          zipperFocusingNextDungeon = goUp zipperWithoutPlayer
          newPosition = upStairs <$> getFocused ds ^. ascendingStairs
          newZipper = case (zipperFocusingNextDungeon, newPlayer, ascendable) of
                          (Just g, Just p, True) ->
                                Just $ modify (\d -> updateMap $ d & actors %~ (:) p) g
                          _ -> Nothing

popPlayer :: TreeZipper Dungeon -> (Maybe Actor, Dungeons)
popPlayer z = (fst $ D.popPlayer $ getFocused z, modify (snd . D.popPlayer) z)

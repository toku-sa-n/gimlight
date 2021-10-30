module Game.Status.Exploring.Dungeons
    ( Dungeons
    , ascendStairsAtPlayerPosition
    , descendStairsAtPlayerPosition
    ) where

import           Control.Lens   ((%~), (&), (.~), (^.))
import           Data.Foldable  (find)
import           Dungeon        (Dungeon, actors, ascendingStairs,
                                 descendingStairs, positionOnParentMap,
                                 updateMap)
import qualified Dungeon        as D
import           Dungeon.Actor  (Actor, position)
import           Dungeon.Stairs (StairsPair (StairsPair, downStairs, upStairs))
import           TreeZipper     (TreeZipper, getFocused, goDownBy, goUp, modify)

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

descendStairsAtPlayerPosition :: Dungeons -> Maybe Dungeons
descendStairsAtPlayerPosition ds = newZipper
    where (player, zipperWithoutPlayer) = popPlayer ds
          newPlayer = case (player, newPosition) of
                          (Just p, Just pos) -> Just $ p & position .~ pos
                          _                  -> Nothing
          zipperFocusingNextDungeon = goDownBy (\x -> x ^. positionOnParentMap == fmap (^. position) player) zipperWithoutPlayer
          newPosition = downStairs <$> find (\(StairsPair from _) -> Just from == fmap (^. position) player) (getFocused ds ^. descendingStairs)
          newZipper = case (zipperFocusingNextDungeon, newPlayer) of
                          (Just g, Just p) -> Just $ modify (\d -> updateMap $ d & actors %~ (:) p) g
                          _ -> Nothing


popPlayer :: TreeZipper Dungeon -> (Maybe Actor, Dungeons)
popPlayer z = (fst $ D.popPlayer $ getFocused z, modify (snd . D.popPlayer) z)

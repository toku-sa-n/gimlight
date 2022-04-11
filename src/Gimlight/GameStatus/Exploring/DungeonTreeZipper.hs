module Gimlight.GameStatus.Exploring.DungeonTreeZipper
    ( Dungeons
    , ascendStairsAtPlayerPosition
    , descendStairsAtPlayerPosition
    , exitDungeon
    , doPlayerAction
    , handleNpcTurns
    ) where

import           Control.Lens               ((%%~), (&), (.~), (^.))
import           Control.Monad.State        (execStateT, runStateT)
import           Control.Monad.Trans.Writer (Writer)
import           Data.Either.Combinators    (rightToMaybe)
import           Data.Foldable              (find)
import           Gimlight.Action            (Action,
                                             ActionResult (killed, newCellMap, status),
                                             ActionStatus)
import           Gimlight.Actor             (Actor, isPlayer)
import           Gimlight.Dungeon           (Dungeon, ascendingStairs, cellMap,
                                             descendingStairs,
                                             positionOnParentMap)
import           Gimlight.Dungeon.Map.Cell  (locateActorAt, playerActor,
                                             removeActorIf, updateExploredMap,
                                             updatePlayerFov)
import           Gimlight.Dungeon.Map.Tile  (TileCollection)
import           Gimlight.Dungeon.Stairs    (StairsPair (StairsPair, downStairs, upStairs))
import           Gimlight.Log               (MessageLog)
import qualified Gimlight.NpcBehavior       as NPC
import           Gimlight.TreeZipper        (TreeZipper, focused, goDownBy,
                                             goUp)

type Dungeons = TreeZipper Dungeon

ascendStairsAtPlayerPosition :: TileCollection -> Dungeons -> Maybe Dungeons
ascendStairsAtPlayerPosition ts ds = newZipper
  where
    (player, zipperWithoutPlayer) = popPlayer ds
    ascendable =
        (downStairs <$> ds ^. focused . ascendingStairs) ==
        (fst <$> playerActor (ds ^. focused . cellMap))
    zipperFocusingNextDungeon = goUp zipperWithoutPlayer
    newPosition = upStairs <$> ds ^. focused . ascendingStairs
    newZipper =
        case (zipperFocusingNextDungeon, newPosition, player, ascendable) of
            (Just g, Just pos, Just p, True) -> updateMapOrError g p pos
            _                                -> Nothing
    updateMapOrError g pos p =
        g & focused . cellMap %%~
        (\x ->
             rightToMaybe (execStateT (locateActorAt ts p pos) x) >>=
             updatePlayerFov ts >>=
             Just .
             updateExploredMap)

descendStairsAtPlayerPosition :: TileCollection -> Dungeons -> Maybe Dungeons
descendStairsAtPlayerPosition ts ds = newZipper
  where
    (player, zipperWithoutPlayer) = popPlayer ds
    zipperFocusingNextDungeon =
        goDownBy
            (\x -> x ^. positionOnParentMap == currentPosition)
            zipperWithoutPlayer
    newPosition =
        downStairs <$>
        find
            (\(StairsPair from _) -> Just from == currentPosition)
            (ds ^. focused . descendingStairs)
    currentPosition = fmap fst . playerActor $ ds ^. focused . cellMap
    newZipper =
        case (zipperFocusingNextDungeon, newPosition, player) of
            (Just g, Just pos, Just p) -> updateMapOrError g p pos
            _                          -> Nothing
    updateMapOrError g pos p =
        g & focused . cellMap %%~
        (\x ->
             rightToMaybe (execStateT (locateActorAt ts p pos) x) >>=
             updatePlayerFov ts >>=
             Just .
             updateExploredMap)

exitDungeon :: TileCollection -> Dungeons -> Maybe Dungeons
exitDungeon ts ds = newZipper
  where
    (player, zipperWithoutPlayer) = popPlayer ds
    currentDungeon = ds ^. focused
    newPosition = currentDungeon ^. positionOnParentMap
    zipperFocusingGlobalMap = goUp zipperWithoutPlayer
    newZipper =
        case (zipperFocusingGlobalMap, newPosition, player) of
            (Just g, Just pos, Just p) ->
                g & focused . cellMap %%~ rightToMaybe .
                execStateT (locateActorAt ts pos p)
            _ -> Nothing

doPlayerAction ::
       Action
    -> TileCollection
    -> Dungeons
    -> Writer MessageLog (ActionStatus, Dungeons, [Actor])
doPlayerAction action ts ds = result
  where
    result = do
        actionResult <- action playerPos ts (ds ^. focused . cellMap)
        let statusAndNewDungeon = (status actionResult, newCellMap actionResult)
        return $
            (\(a, cm) -> (a, ds & focused . cellMap .~ cm, killed actionResult))
                statusAndNewDungeon
    playerPos =
        maybe
            (error "Failed to get the player position")
            fst
            (playerActor $ ds ^. focused . cellMap)

handleNpcTurns ::
       TileCollection -> Dungeons -> Writer MessageLog (Dungeons, [Actor])
handleNpcTurns ts ds =
    (\(x, ks) -> (ds & focused . cellMap .~ x, ks)) <$>
    NPC.handleNpcTurns ts (ds ^. focused . cellMap)

popPlayer :: Dungeons -> (Maybe Actor, Dungeons)
popPlayer z =
    case flip runStateT (z ^. focused . cellMap) $ removeActorIf isPlayer of
        Right (actor, ncm) -> (Just actor, z & focused . cellMap .~ ncm)
        _                  -> (Nothing, z)

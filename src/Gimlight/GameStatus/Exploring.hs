{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Gimlight.GameStatus.Exploring
    ( ExploringHandler
    , exploringHandler
    , quests
    , walkingImages
    , getTileCollection
    , ascendStairsAtPlayerPosition
    , descendStairsAtPlayerPosition
    , exitDungeon
    , doPlayerAction
    , processAfterPlayerTurn
    , getPlayerActor
    , getPlayerPosition
    , currentDungeon
    , getMessageLog
    , getWalkingImageIndex
    , incrementWalkingImageIndex
    ) where

import           Control.Lens                                    (Getter, view)
import           Control.Monad                                   ((>=>))
import           Control.Monad.State                             (execState)
import           Control.Monad.Trans.Writer                      (runWriter)
import           GHC.Generics                                    (Generic)
import           Gimlight.Action                                 (Action,
                                                                  ActionStatus)
import           Gimlight.Actor                                  (Actor,
                                                                  getIdentifier)
import           Gimlight.Actor.WalkingImages                    (WalkingImages,
                                                                  numOfPatterns)
import           Gimlight.Coord                                  (Coord)
import           Gimlight.Dungeon                                (Dungeon,
                                                                  cellMap,
                                                                  identifier)
import           Gimlight.Dungeon.Map.Cell                       (playerActor,
                                                                  updateExploredMap,
                                                                  updatePlayerFov)
import           Gimlight.Dungeon.Map.Tile                       (TileCollection)
import qualified Gimlight.GameStatus.Exploring.DungeonTreeZipper as DS
import           Gimlight.Log                                    (MessageLog)
import qualified Gimlight.Log                                    as L
import           Gimlight.Prelude
import           Gimlight.Quest                                  (QuestCollection,
                                                                  handleWithTurnResult)
import           Gimlight.TreeZipper                             (TreeZipper,
                                                                  focused)

data ExploringHandler =
    ExploringHandler
        { _walkingImageIndex :: Int
        , _dungeons          :: TreeZipper Dungeon
        , _messageLog        :: MessageLog
        , _quests            :: QuestCollection
        , _tileCollection    :: TileCollection
        , _walkingImages'    :: WalkingImages
        }
    deriving (Eq, Generic)

makeLenses ''ExploringHandler

exploringHandler ::
       TreeZipper Dungeon
    -> MessageLog
    -> QuestCollection
    -> TileCollection
    -> WalkingImages
    -> ExploringHandler
exploringHandler = ExploringHandler 0

getTileCollection :: ExploringHandler -> TileCollection
getTileCollection eh = eh ^. tileCollection

ascendStairsAtPlayerPosition :: ExploringHandler -> Maybe ExploringHandler
ascendStairsAtPlayerPosition eh =
    eh & dungeons %%~ DS.ascendStairsAtPlayerPosition (eh ^. tileCollection)

descendStairsAtPlayerPosition :: ExploringHandler -> Maybe ExploringHandler
descendStairsAtPlayerPosition eh =
    eh & dungeons %%~ DS.descendStairsAtPlayerPosition (eh ^. tileCollection)

exitDungeon :: ExploringHandler -> Maybe ExploringHandler
exitDungeon eh = eh & dungeons %%~ DS.exitDungeon (eh ^. tileCollection)

doPlayerAction :: Action -> ExploringHandler -> (ActionStatus, ExploringHandler)
doPlayerAction action eh = (status, newHandler)
  where
    ((status, dungeonsAfterAction, killed), newLog) =
        runWriter $
        DS.doPlayerAction action (eh ^. tileCollection) (eh ^. dungeons)
    newHandler =
        flip execState eh $ do
            messageLog %= L.addMessages newLog
            dungeons .= dungeonsAfterAction
            quests %=
                handleWithTurnResult
                    (dungeonsAfterAction ^. focused . identifier)
                    (map getIdentifier killed)

processAfterPlayerTurn :: ExploringHandler -> Maybe ExploringHandler
processAfterPlayerTurn eh =
    (\x ->
         handlerAfterNpcTurns & dungeons . focused .~ x & quests %~
         updateQuestsForResult (x ^. identifier)) <$>
    newCurrentDungeon
  where
    updateQuestsForResult d = handleWithTurnResult d $ map getIdentifier killed
    newCurrentDungeon =
        (handlerAfterNpcTurns ^. dungeons . focused) & cellMap %%~
        (updatePlayerFov (eh ^. tileCollection) >=> (Just . updateExploredMap))
    (handlerAfterNpcTurns, killed) = handleNpcTurns eh

handleNpcTurns :: ExploringHandler -> (ExploringHandler, [Actor])
handleNpcTurns eh = (newHandler, killed)
  where
    newHandler =
        flip execState eh $ do
            dungeons .= dungeonsAfterNpcTurns
            messageLog %= L.addMessages newLog
    ((dungeonsAfterNpcTurns, killed), newLog) =
        runWriter $ DS.handleNpcTurns (eh ^. tileCollection) (eh ^. dungeons)

getPlayerActor :: ExploringHandler -> Maybe Actor
getPlayerActor = fmap snd . playerActor . view (currentDungeon . cellMap)

getPlayerPosition :: ExploringHandler -> Maybe Coord
getPlayerPosition = fmap fst . playerActor . view (currentDungeon . cellMap)

currentDungeon :: Getter ExploringHandler Dungeon
currentDungeon = dungeons . focused

walkingImages :: Getter ExploringHandler WalkingImages
walkingImages = walkingImages'

getMessageLog :: ExploringHandler -> MessageLog
getMessageLog eh = eh ^. messageLog

-- If `numOfPatterns == 3`, the value of `_walkingImageIndex` changes like
-- 0, 1, 2, 3, 0, 1, ..
-- Here, `getWalkingImageIndex` returns 1 if `_walkingImageIndex == 3`.
getWalkingImageIndex :: ExploringHandler -> Int
getWalkingImageIndex ExploringHandler {_walkingImageIndex = i} =
    if i < numOfPatterns
        then i
        else numOfPatterns - i + 1

incrementWalkingImageIndex :: ExploringHandler -> ExploringHandler
incrementWalkingImageIndex eh = eh & walkingImageIndex %~ (`mod` n) . (+ 1)
  where
    n = 2 * (numOfPatterns - 1)

{-# LANGUAGE DeriveGeneric #-}

module Game.Status.Exploring
    ( ExploringHandler
    , exploringHandler
    , ascendStairsAtPlayerPosition
    , descendStairsAtPlayerPosition
    , exitDungeon
    , doPlayerAction
    , completeThisTurn
    , getPlayerActor
    , getPlayerPosition
    , actorAt
    , isPositionInDungeon
    , addMessages
    , getCurrentDungeon
    , getMessageLog
    ) where

import           Control.Lens                   ((^.))
import           Control.Monad.Trans.Maybe      (MaybeT (runMaybeT))
import           Control.Monad.Trans.Writer     (runWriter)
import           Coord                          (Coord)
import           Data.Binary                    (Binary)
import           Dungeon                        (Dungeon, npcs)
import qualified Dungeon                        as D
import           Dungeon.Actor                  (Actor, position)
import           Dungeon.Actor.Actions          (Action)
import           Dungeon.Actor.Behavior         (npcAction)
import           Dungeon.Turn                   (Status (PlayerKilled))
import           GHC.Generics                   (Generic)
import           Game.Status.Exploring.Dungeons (Dungeons)
import qualified Game.Status.Exploring.Dungeons as DS
import           Log                            (Message, MessageLog)
import qualified Log                            as L
import           TreeZipper                     (TreeZipper, getFocused, modify)

data ExploringHandler = ExploringHandler
                      { dungeons   :: Dungeons
                      , messageLog :: MessageLog
                      } deriving (Show, Ord, Eq, Generic)

instance Binary ExploringHandler

exploringHandler :: TreeZipper Dungeon -> MessageLog -> ExploringHandler
exploringHandler = ExploringHandler

ascendStairsAtPlayerPosition :: ExploringHandler -> Maybe ExploringHandler
ascendStairsAtPlayerPosition eh@ExploringHandler { dungeons = ds } =
    (\x -> eh { dungeons = x }) <$> DS.ascendStairsAtPlayerPosition ds

descendStairsAtPlayerPosition :: ExploringHandler -> Maybe ExploringHandler
descendStairsAtPlayerPosition eh@ExploringHandler { dungeons = ds } =
    (\x -> eh { dungeons = x }) <$> DS.descendStairsAtPlayerPosition ds

exitDungeon :: ExploringHandler -> Maybe ExploringHandler
exitDungeon eh@ExploringHandler { dungeons = ds } =
    (\x -> eh { dungeons = x }) <$> DS.exitDungeon ds

doPlayerAction :: Action -> ExploringHandler -> (Bool, ExploringHandler)
doPlayerAction action ExploringHandler { dungeons = ds, messageLog = l } =
    result
    where (dungeonsAfterAction, newLog) = runWriter $ runMaybeT $ DS.doPlayerAction action ds
          handlerWithNewLog = ExploringHandler { dungeons = ds, messageLog = L.addMessages newLog l }

          result = case dungeonsAfterAction of
                       Just x  -> (True, handlerWithNewLog { dungeons = x })
                       Nothing -> (False, handlerWithNewLog)

completeThisTurn :: ExploringHandler -> Maybe ExploringHandler
completeThisTurn eh =
    if status == PlayerKilled
        then Nothing
        else Just handlerAfterNpcTurns { dungeons = modify (const newCurrentDungeon) $ dungeons handlerAfterNpcTurns }
    where handlerAfterNpcTurns = handleNpcTurns eh
          (status, newCurrentDungeon) = D.completeThisTurn $ getFocused $ dungeons handlerAfterNpcTurns

handleNpcTurns :: ExploringHandler -> ExploringHandler
handleNpcTurns eh = foldl (\acc x -> handleNpcTurn (x ^. position) acc) eh $ npcs $ getCurrentDungeon eh

handleNpcTurn :: Coord -> ExploringHandler -> ExploringHandler
handleNpcTurn c eh@ExploringHandler { dungeons = ds } = newHandler
    where newHandler = case theActor of
                           Just x  -> doAction x
                           Nothing -> error "No such npc."

          theActor = fst . D.popActorAt c $ getFocused ds

          doAction actor = let (newCurrentDungeon, generatedLog) =
                                    runWriter $ runMaybeT $ npcAction actor $ getFocused dungeonsWithoutTheActor
                           in case newCurrentDungeon of
                                   Just d  -> updateDungeonAndLog d generatedLog
                                   Nothing -> eh

          dungeonsWithoutTheActor = modify (snd . D.popActorAt c) ds

          updateDungeonAndLog d l = eh { dungeons = modify (const d) dungeonsWithoutTheActor
                                       , messageLog = L.addMessages l $ getMessageLog eh
                                       }

getPlayerActor :: ExploringHandler -> Maybe Actor
getPlayerActor = D.getPlayerActor . getCurrentDungeon

getPlayerPosition :: ExploringHandler -> Maybe Coord
getPlayerPosition = D.playerPosition . getCurrentDungeon

actorAt :: Coord -> ExploringHandler -> Maybe Actor
actorAt c = D.actorAt c . getCurrentDungeon

isPositionInDungeon :: Coord -> ExploringHandler -> Bool
isPositionInDungeon c = D.isPositionInDungeon c . getCurrentDungeon

addMessages :: [Message] -> ExploringHandler -> ExploringHandler
addMessages newMessages eh = eh { messageLog = L.addMessages newMessages $ getMessageLog eh }

getCurrentDungeon :: ExploringHandler -> Dungeon
getCurrentDungeon ExploringHandler { dungeons = ds } = getFocused ds

getMessageLog :: ExploringHandler -> MessageLog
getMessageLog ExploringHandler { messageLog = l } = l

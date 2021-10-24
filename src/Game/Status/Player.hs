{-# LANGUAGE LambdaCase #-}

module Game.Status.Player
    ( playerBumpAction
    , handlePlayerMoving
    , handlePlayerPickingUp
    , handlePlayerSelectingItemToUse
    , handlePlayerConsumeItem
    ) where

import           Control.Lens                   ((^.))
import           Control.Monad.Trans.State      (State, get, put, runState)
import           Dungeon                        (isTown)
import           Dungeon.Actor                  (Actor, isMonster, talkMessage)
import qualified Dungeon.Actor                  as A
import           Dungeon.Actor.Actions          (consumeAction, meleeAction,
                                                 moveAction, pickUpAction)
import           Game.Status                    (GameStatus (Exploring, GameOver, SelectingItemToUse, Talking))
import           Game.Status.Exploring          (ExploringHandler, actorAt,
                                                 completeThisTurn, doAction,
                                                 getCurrentDungeon,
                                                 getPlayerActor,
                                                 getPlayerPosition,
                                                 isPositionInDungeon)
import qualified Game.Status.Exploring          as GSE
import           Game.Status.SelectingItemToUse (SelectingItemToUseHandler,
                                                 finishSelecting,
                                                 getSelectingIndex,
                                                 selectingItemToUseHandler)
import           Game.Status.Talking            (talkingHandler)
import           Linear.V2                      (V2)
import           Talking                        (talkWith)

playerBumpAction :: V2 Int -> State GameStatus Bool
playerBumpAction offset = do
    gameStatus <- get

    case gameStatus of
        Exploring eh -> do
            let destination = case getPlayerPosition eh of
                                Just p  -> p + offset
                                Nothing -> error "The player is dead."

            case actorAt destination eh of
                Just actorAtDestination -> meleeOrTalk offset actorAtDestination
                Nothing                 -> moveOrExitMap offset
        _ -> error "The player is not exploring."

meleeOrTalk :: V2 Int -> Actor -> State GameStatus Bool
meleeOrTalk offset target = do
    gameStatus <- get

    case gameStatus of
        Exploring eh ->
            if isMonster target
                then let (newStatus, isSuccess) = doAction (meleeAction offset) eh
                     in do
                         put $ Exploring newStatus
                         return isSuccess
                else do
                    put $ Talking $ talkingHandler (talkWith target $ target ^. talkMessage) eh
                    return True
        _ -> error "We are not exploring."

moveOrExitMap :: V2 Int -> State GameStatus Bool
moveOrExitMap offset = do
    gameStatus <- get

    case gameStatus of
        Exploring eh -> do
            let destination = case getPlayerPosition eh of
                                Just p  -> p + offset
                                Nothing -> error "The player is dead."

            if isPositionInDungeon destination eh || not (isTown (getCurrentDungeon eh))
                then let (newStatus, isSuccess) = doAction (moveAction offset) eh
                     in do
                         put $ Exploring newStatus
                         return isSuccess
                else do
                    put $ exitDungeon eh
                    return True
        _ -> error "The player is not exploring."

exitDungeon :: ExploringHandler -> GameStatus
exitDungeon eh =
    case GSE.exitDungeon eh of
        Just newEh -> Exploring newEh
        Nothing    -> error "Failed to exit from the dungeon."

handlePlayerMoving :: V2 Int -> GameStatus -> GameStatus
handlePlayerMoving offset gs =
    let (isSuccess, newState) = runState (playerBumpAction offset) gs
    in if isSuccess
        then case newState of
                 Exploring eh -> maybe GameOver Exploring (completeThisTurn eh)
                 _            -> newState
        else newState


handlePlayerPickingUp :: ExploringHandler -> GameStatus
handlePlayerPickingUp eh =
    let (newHandler, isSuccess) = doAction pickUpAction eh
    in if isSuccess
        then maybe GameOver Exploring $ completeThisTurn newHandler
        else Exploring newHandler

handlePlayerSelectingItemToUse :: ExploringHandler -> GameStatus
handlePlayerSelectingItemToUse eh =
    SelectingItemToUse $ selectingItemToUseHandler xs eh
    where xs = A.getItems p
          p = case getPlayerActor eh of
                Just x  -> x
                Nothing -> error "Player is dead."

handlePlayerConsumeItem :: SelectingItemToUseHandler -> GameStatus
handlePlayerConsumeItem sh =
    case getSelectingIndex sh of
        Just n ->
            let (newHandler, isSuccess) = doAction (consumeAction n) $ finishSelecting sh
            in if isSuccess
                then maybe GameOver Exploring (completeThisTurn newHandler)
                else Exploring newHandler
        Nothing -> SelectingItemToUse sh

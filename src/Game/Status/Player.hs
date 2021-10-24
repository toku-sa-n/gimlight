{-# LANGUAGE LambdaCase #-}
module Game.Status.Player
    ( playerBumpAction
    , handlePlayerMoving
    , handlePlayerPickingUp
    , handlePlayerSelectingItemToUse
    , handlePlayerConsumeItem
    ) where
import           Control.Lens                   ((^.))
import           Control.Monad                  (when)
import           Control.Monad.Trans.State      (State, get, put, state)
import           Data.Bifunctor                 (Bifunctor (first))
import           Dungeon                        (isTown)
import           Dungeon.Actor                  (Actor, isMonster, talkMessage)
import qualified Dungeon.Actor                  as A
import           Dungeon.Actor.Actions          (Action, consumeAction,
                                                 meleeAction, moveAction,
                                                 pickUpAction)
import           Game.Status                    (GameStatus (Exploring, GameOver, SelectingItemToUse, Talking))
import           Game.Status.Exploring          (actorAt, completeThisTurn,
                                                 getCurrentDungeon,
                                                 getPlayerActor,
                                                 getPlayerPosition,
                                                 isPositionInDungeon)
import qualified Game.Status.Exploring          as GSE
import           Game.Status.SelectingItemToUse (finishSelecting,
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
                then let (newStatus, isSuccess) = doAction (meleeAction offset) gameStatus
                     in do
                         put newStatus
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
                then let (newStatus, isSuccess) = doAction (moveAction offset) gameStatus
                     in do
                         put newStatus
                         return isSuccess
                else do
                    exitDungeon
                    return True
        _ -> error "The player is not exploring."

exitDungeon :: State GameStatus ()
exitDungeon = state $ \case
    Exploring eh -> case GSE.exitDungeon eh of
                        Just newEh -> ((), Exploring newEh)
                        Nothing    -> error "Failed to exit from the dungeon."
    _ -> undefined

handlePlayerMoving :: V2 Int -> State GameStatus ()
handlePlayerMoving offset = do
    eng <- get

    case eng of
        GameOver -> return ()
        _ -> do
            success <- playerBumpAction offset

            when success $ do
                eng' <- get

                case eng' of
                    Exploring eh -> case completeThisTurn eh of
                                        Just afterEh -> put $ Exploring afterEh
                                        Nothing      -> put GameOver
                    _            -> return ()


handlePlayerPickingUp :: State GameStatus ()
handlePlayerPickingUp = do
    eng <- get

    case eng of
        GameOver -> return ()
        _ -> let (newStatus, isSuccess) = doAction pickUpAction eng
             in do
                put newStatus
                when isSuccess $ do
                    eng' <- get

                    case eng' of
                        Exploring eh -> case completeThisTurn eh of
                                            Just afterEh -> put $ Exploring afterEh
                                            Nothing      -> put GameOver
                        _ -> return ()

handlePlayerSelectingItemToUse :: GameStatus -> GameStatus
handlePlayerSelectingItemToUse (Exploring eh) =
    SelectingItemToUse $ selectingItemToUseHandler xs eh
    where xs = A.getItems p
          p = case getPlayerActor eh of
                Just x  -> x
                Nothing -> error "Player is dead."
handlePlayerSelectingItemToUse _ = undefined

handlePlayerConsumeItem :: State GameStatus ()
handlePlayerConsumeItem = do
    gs <- get

    case gs of
        SelectingItemToUse sh -> do
            case getSelectingIndex sh of
                Just n -> do
                    put $ Exploring $ finishSelecting sh

                    gs' <- get
                    let (newStatus, isSuccess) = doAction (consumeAction n) gs'

                    put newStatus

                    when isSuccess $ do
                        gs'' <- get

                        case gs'' of
                            Exploring eh -> case completeThisTurn eh of
                                Just afterEh -> put $ Exploring afterEh
                                Nothing      -> put GameOver
                            _ -> return ()
                Nothing -> return ()
        _ -> error "We are not selecting an item."

doAction :: Action -> GameStatus -> (GameStatus, Bool)
doAction action (Exploring eh) =
    first Exploring $ GSE.doAction action eh
doAction _ _ = error "We are not exploring a dungeon."

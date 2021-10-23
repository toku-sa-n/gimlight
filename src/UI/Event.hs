{-# LANGUAGE OverloadedStrings #-}

module UI.Event
    ( handleEvent
    ) where

import           Data.Text   (Text)
import           Game        (Game (Game, config, status),
                              handlePlayerConsumingItem,
                              handlePlayerEnteringTown, handlePlayerMoving,
                              handlePlayerPickingUp,
                              handlePlayerSelectingItemToUse, isHandlingScene,
                              isPlayerExploring, isPlayerTalking,
                              isSelectingItemToUse, isSelectingLocale, isTitle,
                              saveStatus)
import           Game.Config (Language (English, Japanese), setLocale)
import           Game.Status (finishSelecting, finishTalking, newGameStatus,
                              nextSceneElementOrFinish, selectNextItem,
                              selectPrevItem)
import           Linear.V2   (V2 (V2))
import           Monomer     (AppEventResponse, EventResponse (Model, Task),
                              WidgetEnv, WidgetNode, exitApplication)
import           Save        (load)
import           UI.Types    (AppEvent (AppInit, AppKeyboardInput, AppLoadFinished, AppSaveFinished))

handleEvent :: WidgetEnv Game AppEvent -> WidgetNode Game AppEvent -> Game -> AppEvent -> [AppEventResponse Game AppEvent]
handleEvent _ _ gameStatus evt =
    case evt of
        AppInit             -> []
        AppSaveFinished     -> []
        AppLoadFinished ngs -> [Model ngs]
        AppKeyboardInput k  -> handleKeyInput gameStatus k

handleKeyInput :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInput e k
    | isPlayerExploring e = handleKeyInputDuringExploring e k
    | isPlayerTalking e = handleKeyInputDuringTalking e k
    | isHandlingScene e = handleKeyInputDuringHandlingScene e k
    | isSelectingItemToUse e = handleKeyInputDuringSelectingItemToUse e k
    | isTitle e = handleKeyInputDuringTitle e k
    | isSelectingLocale e = handleKeyInputDuringSelectingLanguage e k
    | otherwise = undefined

handleKeyInputDuringExploring :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringExploring e k
    | k == "Right" = [Model $ handlePlayerMoving (V2 1 0) e]
    | k == "Left"  = [Model $ handlePlayerMoving (V2 (-1) 0) e]
    | k == "Up"    = [Model $ handlePlayerMoving (V2 0 1) e]
    | k == "Down"  = [Model $ handlePlayerMoving (V2 0 (-1)) e]
    | k == "g" = [Model $ handlePlayerPickingUp e]
    | k == "u" = [Model $ handlePlayerSelectingItemToUse e]
    | k == "Ctrl-s"     = [Task (saveStatus e >> return AppSaveFinished)]
    | k == "Ctrl-l"     = [Task $ do
                            s <- load
                            return $ AppLoadFinished e { status = s }]
    | k == "Enter" = [Model $ handlePlayerEnteringTown e]
    | otherwise = []

handleKeyInputDuringTalking :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringTalking e@Game { status = s } k
    | k == "Enter" = [Model $ e { status = finishTalking s }]
    | otherwise = []

handleKeyInputDuringHandlingScene :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringHandlingScene e@Game { status = s } k
    | k == "Enter" = [Model $ e { status = nextSceneElementOrFinish s }]
    | otherwise = []

handleKeyInputDuringSelectingItemToUse :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringSelectingItemToUse e@Game { status = s } k
    | k == "Up" = [Model $ e { status = selectPrevItem s }]
    | k == "Down" = [Model $ e { status = selectNextItem s }]
    | k == "Enter" = [Model $ handlePlayerConsumingItem e]
    | k == "Esc" = [Model $ e { status =  finishSelecting s }]
    | otherwise = []

handleKeyInputDuringTitle :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringTitle g k
    | k == "n" = [Task $ AppLoadFinished <$> startNewGame g]
    | k == "l" = [Task $ do
                    s <- load
                    return $ AppLoadFinished g { status = s }]
    | k == "q" = [exitApplication]
    | otherwise = []
    where startNewGame Game { config = c } =
            do
                st <- newGameStatus
                return Game { status = st
                            , config = c
                            }

handleKeyInputDuringSelectingLanguage :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringSelectingLanguage g@Game { config = c } k
    | k == "e" = [Task $ return $ AppLoadFinished g { config = setLocale English c }]
    | k == "j" = [Task $ return $ AppLoadFinished g { config = setLocale Japanese c }]
    | otherwise = []

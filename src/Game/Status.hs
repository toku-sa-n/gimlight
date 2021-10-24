{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Status
    ( GameStatus(..)
    , isPlayerExploring
    , isPlayerTalking
    , isHandlingScene
    , isSelectingItemToUse
    , isTitle
    , isGameOver
    , isSelectingLocale
    , nextSceneElementOrFinish
    , newGameStatus
    ) where

import           Data.Binary                    (Binary)
import           Dungeon.Init                   (initDungeon)
import           Dungeon.Predefined.BatsCave    (batsDungeon)
import           Dungeon.Predefined.GlobalMap   (globalMap)
import           GHC.Generics                   (Generic)
import           Game.Status.Exploring          (ExploringHandler,
                                                 exploringHandler)
import           Game.Status.Scene              (SceneHandler, sceneHandler)
import qualified Game.Status.Scene              as GSS
import           Game.Status.SelectingItemToUse (SelectingItemToUseHandler)
import           Game.Status.Talking            (TalkingHandler)
import           Localization                   (multilingualText)
import qualified Log                            as L
import           Scene                          (gameStartScene)
import           System.Random                  (getStdGen)

data GameStatus = Exploring ExploringHandler
                | Talking TalkingHandler
                | HandlingScene SceneHandler
                | SelectingItemToUse SelectingItemToUseHandler
                | Title
                | GameOver
                | SelectingLocale
                deriving (Show, Ord, Eq, Generic)
instance Binary GameStatus

isPlayerExploring :: GameStatus -> Bool
isPlayerExploring Exploring{} = True
isPlayerExploring _           = False

isPlayerTalking :: GameStatus -> Bool
isPlayerTalking Talking{} = True
isPlayerTalking _         = False

isHandlingScene :: GameStatus -> Bool
isHandlingScene HandlingScene{} = True
isHandlingScene _               = False

isSelectingItemToUse :: GameStatus -> Bool
isSelectingItemToUse SelectingItemToUse{} = True
isSelectingItemToUse _                    = False

isTitle :: GameStatus -> Bool
isTitle Title = True
isTitle _     = False

isGameOver :: GameStatus -> Bool
isGameOver GameOver = True
isGameOver _        = False

isSelectingLocale :: GameStatus -> Bool
isSelectingLocale SelectingLocale = True
isSelectingLocale _               = False

nextSceneElementOrFinish :: GameStatus -> GameStatus
nextSceneElementOrFinish (HandlingScene sh) = case GSS.nextSceneOrFinish sh of
    Right newSh -> HandlingScene newSh
    Left after  -> Exploring after
nextSceneElementOrFinish _                   = error "We are not handling a scene."

newGameStatus :: IO GameStatus
newGameStatus = do
    g <- getStdGen

    let bats = batsDungeon g
        initExploring = exploringHandler
            initDungeon
            [globalMap, bats] $
            foldr (L.addMessage . L.message) L.emptyLog
                [multilingualText "Welcome to a roguelike game!" "ローグライクゲームへようこそ！"]

    return $ HandlingScene $ sceneHandler gameStartScene initExploring

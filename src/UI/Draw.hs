{-# LANGUAGE OverloadedStrings #-}

module UI.Draw
    ( drawUI
    ) where

import           Control.Lens                   ((^.))
import qualified Dungeon.Item                   as I
import           Game                           (Game (Game, config, status))
import           Game.Status                    (GameStatus (Exploring, GameOver, HandlingScene, SelectingItemToUse, SelectingLocale, Talking, Title))
import qualified Game.Status.Scene              as GSS
import           Game.Status.SelectingItemToUse (getItems, getSelectingIndex)
import           Localization                   (getLocalizedText,
                                                 multilingualText)
import           Monomer                        (CmbMultiline (multiline),
                                                 CmbStyleBasic (styleBasic),
                                                 CmbTextColor (textColor),
                                                 CmbTextSize (textSize), black,
                                                 image, label, label_, vstack,
                                                 zstack)
import           Scene                          (backgroundImage, elements,
                                                 text)
import           TextShow                       (TextShow (showt))
import           UI.Draw.Exploring              (drawExploring)
import           UI.Draw.KeyEvent               (withKeyEvents)
import           UI.Draw.Talking                (drawTalking)
import           UI.Types                       (GameWidgetEnv, GameWidgetNode)

drawUI :: GameWidgetEnv -> Game -> GameWidgetNode
drawUI _ gs@Game { status = s } =
    case s of
        Exploring _          -> drawExploring gs
        Talking _            -> drawTalking gs
        HandlingScene _      -> drawHandlingScene gs
        SelectingItemToUse _ -> drawSelectingItem gs
        Title                -> drawTitle gs
        GameOver             -> drawGameOver
        SelectingLocale      -> drawSelectingLanguage

drawHandlingScene :: Game -> GameWidgetNode
drawHandlingScene Game { status = HandlingScene sh, config = c } =
    withKeyEvents $ zstack [ image (s ^. backgroundImage)
                           , label_  (getLocalizedText c $ text $ head $ s ^. elements) [multiline] `styleBasic` [textColor black]
                           ]
    where (s, _) = GSS.destructHandler sh
drawHandlingScene _ = error "We are not handling a scene."

drawSelectingItem :: Game -> GameWidgetNode
drawSelectingItem Game { status = SelectingItemToUse sh, config = c } = withKeyEvents $ vstack labels
    where labels = label topLabel:map label addAsterlist
          addAsterlist = zipWith (\idx x -> if Just idx == getSelectingIndex sh
                                                then "* " <> showt idx <> " " <> x
                                                else showt idx <> " " <> x
                                               ) [0..] $ map (getLocalizedText c) itemNames
          itemNames = map (^. I.name) $ getItems sh
          topLabel = getLocalizedText c $ multilingualText "Which Item do you use?" "どのアイテムを使う？"
drawSelectingItem _ = error "We are not selecting an item."

drawSelectingLanguage :: GameWidgetNode
drawSelectingLanguage = withKeyEvents $ vstack [ label "Choose your language. / 言語を選択してください．"
                                               , label "[e] English"
                                               , label "[j] 日本語"
                                               ]

drawTitle :: Game -> GameWidgetNode
drawTitle Game { config = c } = withKeyEvents $ vstack [ label "Gimlight" `styleBasic` [textSize 36]
                                     , label $ "[n] " <> getLocalizedText c newGame
                                     , label $ "[l] " <> getLocalizedText c loadGame
                                     , label $ "[q] " <> getLocalizedText c quitGame
                                     ]
    where newGame = multilingualText "New game" "新しく始める"
          loadGame = multilingualText " Load the savedata" "セーブデータを読み込む"
          quitGame = multilingualText "Quit" "終了する"

drawGameOver :: GameWidgetNode
drawGameOver = vstack [label "Game Over" `styleBasic` [textSize 72]]

{-# LANGUAGE OverloadedStrings #-}

module UI.Draw
    ( drawUI
    ) where

import           Control.Lens                   ((&), (.~), (^.))
import           Dungeon.Actor                  (standingImagePath)
import qualified Dungeon.Item                   as I
import           Game                           (Game (Game, config, status))
import           Game.Status                    (GameStatus (Exploring, GameOver, HandlingScene, SelectingItemToUse, SelectingLocale, Talking, Title))
import qualified Game.Status.Scene              as GSS
import           Game.Status.SelectingItemToUse (getItems, getSelectingIndex)
import qualified Game.Status.Talking            as GST
import           Localization                   (getLocalizedText,
                                                 multilingualText)
import           Monomer                        (CmbBgColor (bgColor),
                                                 CmbMultiline (multiline),
                                                 CmbPaddingL (paddingL),
                                                 CmbStyleBasic (styleBasic),
                                                 CmbTextColor (textColor),
                                                 CmbTextSize (textSize), black,
                                                 filler, gray, hstack, image,
                                                 label, label_, red, vstack,
                                                 zstack)
import qualified Monomer.Graphics.Lens          as L
import           Scene                          (backgroundImage, elements,
                                                 text)
import           Talking                        (TalkWith, message, person)
import           TextShow                       (TextShow (showt))
import           UI.Draw.Exploring              (drawExploring)
import           UI.Draw.KeyEvent               (withKeyEvents)
import           UI.Types                       (GameWidgetEnv, GameWidgetNode)

drawUI :: GameWidgetEnv -> Game -> GameWidgetNode
drawUI wenv gs@Game { status = s } =
    case s of
        Exploring _          -> drawExploring gs
        Talking _            -> drawTalking wenv gs
        HandlingScene _      -> drawHandlingScene gs
        SelectingItemToUse _ -> drawSelectingItem gs
        Title                -> drawTitle gs
        GameOver             -> drawGameOver
        SelectingLocale      -> drawSelectingLanguage

drawTalking ::  GameWidgetEnv -> Game -> GameWidgetNode
drawTalking wenv e@Game { status = Talking th } =
    withKeyEvents $ zstack [ drawUI wenv (e { status = Exploring afterGameStatus }) `styleBasic` [bgColor $ gray & L.a .~ 0.5]
                           , filler `styleBasic` [bgColor $ black & L.a .~ 0.5]
                           , talkingWindow e with
                           ]
    where (with, afterGameStatus) = GST.destructHandler th
drawTalking _ _ = error "We are not handling a talk event."

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

talkingWindow :: Game -> TalkWith -> GameWidgetNode
talkingWindow Game { config = c } tw = hstack [ image (tw ^. person . standingImagePath)
                            , window
                            ]
    where window = zstack [ image "images/talking_window.png"
                          , label (getLocalizedText c (tw ^. message)) `styleBasic` [textColor red, textSize 16, paddingL 50]
                          ]

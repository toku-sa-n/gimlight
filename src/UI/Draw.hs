{-# LANGUAGE OverloadedStrings #-}

module UI.Draw
    ( drawUI
    ) where

import           Control.Lens                   ((^.))
import qualified Dungeon.Item                   as I
import           Game                           (Game (Game, config, status))
import           Game.Status                    (GameStatus (Exploring, GameOver, HandlingScene, SelectingItemToUse, SelectingLocale, Talking, Title))
import           Game.Status.SelectingItemToUse (getItems, getSelectingIndex)
import           Localization                   (getLocalizedText,
                                                 multilingualText)
import           Monomer                        (label, vstack)
import           TextShow                       (TextShow (showt))
import           UI.Draw.Exploring              (drawExploring)
import           UI.Draw.GameOver               (drawGameOver)
import           UI.Draw.KeyEvent               (withKeyEvents)
import           UI.Draw.Scene                  (drawScene)
import           UI.Draw.Talking                (drawTalking)
import           UI.Draw.Title                  (drawTitle)
import           UI.Types                       (GameWidgetEnv, GameWidgetNode)

drawUI :: GameWidgetEnv -> Game -> GameWidgetNode
drawUI _ gs@Game { status = s } =
    case s of
        Exploring _          -> drawExploring gs
        Talking _            -> drawTalking gs
        HandlingScene _      -> drawScene gs
        SelectingItemToUse _ -> drawSelectingItem gs
        Title                -> drawTitle gs
        GameOver             -> drawGameOver
        SelectingLocale      -> drawSelectingLanguage

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

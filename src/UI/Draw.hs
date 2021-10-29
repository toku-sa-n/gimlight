module UI.Draw
    ( drawUI
    ) where

import           Game                    (Game (Game, config, status))
import           Game.Status             (GameStatus (Exploring, GameOver, HandlingScene, SelectingItemToUse, SelectingLocale, Talking, Title))
import           UI.Draw.Exploring       (drawExploring)
import           UI.Draw.GameOver        (drawGameOver)
import           UI.Draw.Scene           (drawScene)
import           UI.Draw.SelectingItem   (drawSelectingItem)
import           UI.Draw.SelectingLocale (drawSelectingLocale)
import           UI.Draw.Talking         (drawTalking)
import           UI.Draw.Title           (drawTitle)
import           UI.Types                (GameWidgetEnv, GameWidgetNode)

drawUI :: GameWidgetEnv -> Game -> GameWidgetNode
drawUI _ gs@Game { status = s, config = c } =
    case s of
        Exploring eh         -> drawExploring eh c
        Talking th           -> drawTalking th c
        HandlingScene _      -> drawScene gs
        SelectingItemToUse _ -> drawSelectingItem gs
        Title                -> drawTitle gs
        GameOver             -> drawGameOver
        SelectingLocale      -> drawSelectingLocale

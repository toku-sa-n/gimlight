module UI.Draw
    ( drawUI
    ) where
import           Engine      (Engine)
import           Monomer     (CmbStyleBasic (styleBasic), CmbWidth (width),
                              WidgetEnv, WidgetNode, vstack)
import           UI.Draw.Map (mapGrid)
import           UI.Types    (AppEvent)

drawUI :: WidgetEnv Engine AppEvent -> Engine -> WidgetNode Engine AppEvent
drawUI _ _ = vstack [ mapGrid
                    ] `styleBasic` [width 0]

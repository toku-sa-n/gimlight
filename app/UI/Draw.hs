module UI.Draw
    ( drawUI
    ) where
import           Data.Text (pack)
import           Engine    (Engine)
import           Monomer   (WidgetEnv, WidgetNode, label, vstack)
import           UI.Types  (AppEvent)

drawUI :: WidgetEnv Engine AppEvent -> Engine -> WidgetNode Engine AppEvent
drawUI _ _ = vstack [ label $ pack "hello, world"
                    ]

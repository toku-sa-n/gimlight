module UI.Event
    ( handleEvent
    ) where

import           Engine   (Engine)
import           Monomer  (AppEventResponse, WidgetEnv, WidgetNode)
import           UI.Types (AppEvent)

handleEvent :: WidgetEnv Engine AppEvent -> WidgetNode Engine AppEvent -> Engine -> AppEvent -> [AppEventResponse Engine AppEvent]
handleEvent _ _ _ _ = []

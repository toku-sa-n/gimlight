module Gimlight.UI.Draw.Containers (shadeBackWindow) where

import Control.Lens ((&), (.~))
import Monomer (CmbBgColor (bgColor), CmbStyleBasic (styleBasic), WidgetEvent, WidgetModel, WidgetNode, black, filler, zstack)
import qualified Monomer.Lens as L

shadeBackWindow :: (WidgetModel s, WidgetEvent e) => WidgetNode s e -> WidgetNode s e
shadeBackWindow inner = zstack [filler `styleBasic` [bgColor $ black & L.a .~ 0.5], inner]

module Gimlight.UI.Draw.Scene
  ( drawScene,
  )
where

import Control.Lens ((&), (.~))
import Gimlight.GameConfig (GameConfig)
import Gimlight.GameStatus.Scene
  ( SceneHandler,
    getBackgroundImagePath,
    getCurrentScene,
    text,
  )
import Gimlight.Localization (getLocalizedText)
import Gimlight.UI.Draw.KeyEvent (withKeyEvents)
import Gimlight.UI.Types (GameWidgetNode)
import Monomer
  ( CmbAlignBottom (alignBottom),
    CmbBgColor (bgColor),
    CmbHeight (height),
    CmbMultiline (multiline),
    CmbStyleBasic (styleBasic),
    CmbTextColor (textColor),
    box_,
    filler,
    gray,
    image,
    label_,
    vstack,
    white,
    zstack,
  )
import qualified Monomer.Lens as L

drawScene :: SceneHandler -> GameConfig -> GameWidgetNode
drawScene sh c =
  withKeyEvents $
    zstack
      [ image $ getBackgroundImagePath sh,
        box_ [alignBottom] (drawText sh c)
          `styleBasic` [height 10]
      ]

drawText :: SceneHandler -> GameConfig -> GameWidgetNode
drawText sh c =
  vstack
    [ filler,
      zstack
        [ filler `styleBasic` [bgColor $ gray & L.a .~ 0.5],
          label_ (getLocalizedText c $ text $ getCurrentScene sh) [multiline]
            `styleBasic` [textColor white]
        ]
        `styleBasic` [height 200]
    ]

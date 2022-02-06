{-# LANGUAGE OverloadedStrings #-}

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
import Gimlight.UI.Draw.Fonts (bold)
import Gimlight.UI.Draw.KeyEvent (withKeyEvents)
import Gimlight.UI.Types (AppEvent (ShowNextScene), GameWidgetNode)
import Monomer
  ( CmbAlignBottom (alignBottom),
    CmbAlignLeft (alignLeft),
    CmbAlignTop (alignTop),
    CmbBgColor (bgColor),
    CmbHeight (height),
    CmbMultiline (multiline),
    CmbOnFinished (onFinished),
    CmbPadding (padding),
    CmbStyleBasic (styleBasic),
    CmbTextColor (textColor),
    CmbTextFont (textFont),
    CmbTextSize (textSize),
    WidgetNode,
    animFadeIn,
    animFadeOut_,
    black,
    box_,
    filler,
    image,
    label_,
    nodeKey,
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
drawText sh c = widgetTree
  where
    widgetTree =
      vstack
        [ filler,
          zstack
            [ filler `styleBasic` [bgColor $ black & L.a .~ 0.5],
              fadeInOut $
                box_ [alignTop, alignLeft] $
                  label_ (getLocalizedText c $ text $ getCurrentScene sh) [multiline]
                    `styleBasic` [textColor white, textSize 20, padding 20, textFont bold]
            ]
            `styleBasic` [height 200]
        ]

fadeInOut :: WidgetNode s AppEvent -> WidgetNode s AppEvent
fadeInOut content = outer
  where
    inner = animFadeIn content `nodeKey` "sceneFadeIn"
    outer = animFadeOut_ [onFinished ShowNextScene] inner `nodeKey` "sceneFadeOut"

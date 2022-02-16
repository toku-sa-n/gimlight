{-# LANGUAGE OverloadedStrings #-}

module Gimlight.UI.Draw.SelectingItem
    ( drawSelectingItem
    ) where

import           Control.Lens                      ((&), (.~))
import           Gimlight.GameConfig               (GameConfig)
import           Gimlight.GameStatus.SelectingItem (Reason (Drop, Use),
                                                    SelectingItemHandler,
                                                    getExploringHandler,
                                                    getItems, getReason,
                                                    getSelectingIndex)
import           Gimlight.Item                     (getName)
import           Gimlight.Localization             (getLocalizedText)
import qualified Gimlight.Localization.Texts       as T
import           Gimlight.UI.Draw.Exploring        (drawExploring)
import           Gimlight.UI.Draw.Fonts            (bold)
import           Gimlight.UI.Draw.KeyEvent         (withKeyEvents)
import           Gimlight.UI.Types                 (GameWidgetNode)
import           Monomer                           (CmbAlignCenter (alignCenter),
                                                    CmbAlignMiddle (alignMiddle),
                                                    CmbBgColor (bgColor),
                                                    CmbBorderB (borderB),
                                                    CmbHeight (height),
                                                    CmbPadding (padding),
                                                    CmbStyleBasic (styleBasic),
                                                    CmbTextColor (textColor),
                                                    CmbTextFont (textFont),
                                                    CmbTextSize (textSize),
                                                    CmbWidth (width),
                                                    Color (Color), black, box_,
                                                    filler, label, paddingV,
                                                    vstack, white, zstack)
import qualified Monomer.Lens                      as L
import           TextShow                          (TextShow (showt))

drawSelectingItem :: SelectingItemHandler -> GameConfig -> GameWidgetNode
drawSelectingItem sh c =
    withKeyEvents $
    zstack
        [ drawExploring eh c
        , filler `styleBasic` [bgColor $ black & L.a .~ 0.5]
        , box_
              [alignMiddle, alignCenter]
              (vstack labels `styleBasic`
               [ width 800
               , height 600
               , bgColor (Color 0x0c 0x0c 0x0c 1)
               , padding 24
               ])
        ]
  where
    eh = getExploringHandler sh
    labels =
        (label topLabel `styleBasic`
         [ textColor white
         , textSize 32
         , textFont bold
         , borderB 1 white
         , paddingV 4
         ]) :
        map label addAsterlist
    addAsterlist =
        zipWith
            (\idx x ->
                 if Just idx == getSelectingIndex sh
                     then "* " <> showt idx <> " " <> x
                     else showt idx <> " " <> x)
            [0 ..] $
        map (getLocalizedText c) itemNames
    itemNames = map getName $ getItems sh
    topLabel = getLocalizedText c topLabelText
    topLabelText =
        case getReason sh of
            Use  -> T.whatToUse
            Drop -> T.whatToDrop

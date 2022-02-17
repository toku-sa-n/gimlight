module Gimlight.UI.Draw.SelectingItem
    ( drawSelectingItem
    ) where

import           Control.Lens                      (Ixed (ix), (%~), (&))
import           Gimlight.GameConfig               (GameConfig)
import           Gimlight.GameStatus.SelectingItem (Reason (Drop, Use),
                                                    SelectingItemHandler,
                                                    getExploringHandler,
                                                    getItems, getReason,
                                                    getSelectingIndex)
import           Gimlight.Item                     (getName)
import           Gimlight.Localization             (MultilingualText,
                                                    getLocalizedText)
import qualified Gimlight.Localization.Texts       as T
import           Gimlight.UI.Draw.Config           (windowHeight, windowWidth)
import           Gimlight.UI.Draw.Exploring        (drawExploring)
import           Gimlight.UI.Draw.Fonts            (bold)
import           Gimlight.UI.Draw.KeyEvent         (withKeyEvents)
import           Gimlight.UI.Draw.Shadow           (shadow)
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
                                                    Color (Color), StyleState,
                                                    box_, label, paddingV,
                                                    vstack, white, zstack)

drawSelectingItem :: SelectingItemHandler -> GameConfig -> GameWidgetNode
drawSelectingItem sh c =
    withKeyEvents $
    zstack
        [ drawExploring eh c
        , shadow
        , box_
              [alignMiddle, alignCenter]
              (vstack labels `styleBasic`
               [ width $ fromIntegral windowWidth * 0.8
               , height $ fromIntegral windowHeight * 0.8
               , bgColor (Color 0x0c 0x0c 0x0c 1)
               , padding 24
               ])
        ]
  where
    eh = getExploringHandler sh
    labels = titleLabel sh c : itemLabels
    itemLabels =
        emphasizeSelection sh $
        fmap
            (\x -> label (getLocalizedText c x) `styleBasic` baseStyle)
            itemNames
    itemNames = map getName $ getItems sh

emphasizeSelection ::
       SelectingItemHandler -> [GameWidgetNode] -> [GameWidgetNode]
emphasizeSelection h labels =
    case getSelectingIndex h of
        Just x  -> labels & ix x %~ (`styleBasic` baseStyle <> [textFont bold])
        Nothing -> labels

titleLabel :: SelectingItemHandler -> GameConfig -> GameWidgetNode
titleLabel sh c = node `styleBasic` style
  where
    node = label $ getLocalizedText c $ titleLabelText sh
    style =
        [ textColor white
        , textSize 32
        , textFont bold
        , borderB 1 white
        , paddingV 4
        ]

titleLabelText :: SelectingItemHandler -> MultilingualText
titleLabelText sh =
    case getReason sh of
        Use  -> T.whatToUse
        Drop -> T.whatToDrop

baseStyle :: [StyleState]
baseStyle = [textColor white, textSize 24]

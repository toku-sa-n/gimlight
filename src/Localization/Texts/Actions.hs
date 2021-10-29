{-# LANGUAGE OverloadedStrings #-}

module Localization.Texts.Actions
    ( youCannotMoveThere
    , youGotItem
    , youGotNohing
    , bagIsFull
    , whatToUse
    , healed
    ) where

import           Localization (MultilingualText, multilingualText)
import           TextShow     (TextShow (showt))

youCannotMoveThere :: MultilingualText
youCannotMoveThere = multilingualText "That way is blocked." "その方向には進めない．"

youGotItem :: MultilingualText -> MultilingualText
youGotItem item =
       multilingualText "You got " ""
    <> item
    <> multilingualText "." "を入手した．"

youGotNohing :: MultilingualText
youGotNohing = multilingualText "You got nothing." "あなたは無を入手した．"

bagIsFull :: MultilingualText
bagIsFull = multilingualText "Your bag is full." "バッグは一杯だ．"

healed :: MultilingualText -> Int -> MultilingualText
healed who amount =
    who
    <> multilingualText " healed " "は"
    <> amount''
    <> multilingualText " point." "ポイント回復した．"
    where amount'' = multilingualText amount' amount'
          amount' = showt amount


whatToUse :: MultilingualText
whatToUse = multilingualText "What do you consume" "何を使う？"

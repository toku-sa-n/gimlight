{-# LANGUAGE OverloadedStrings #-}

module Localization.Texts.Actions
    ( youCannotMoveThere
    , youGotItem
    , youGotNohing
    , bagIsFull
    ) where

import           Localization (MultilingualText, multilingualText)

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

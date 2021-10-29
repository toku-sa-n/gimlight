{-# LANGUAGE OverloadedStrings #-}

module Localization.Texts.Actions
    ( youGotItem
    , youGotNohing
    , bagIsFull
    ) where

import           Localization (MultilingualText, multilingualText)

youGotItem :: MultilingualText -> MultilingualText
youGotItem item =
       multilingualText "You got " ""
    <> item
    <> multilingualText "." "を入手した．"

youGotNohing :: MultilingualText
youGotNohing = multilingualText "You got nothing." "あなたは無を入手した．"

bagIsFull :: MultilingualText
bagIsFull = multilingualText "Your bag is full." "バッグは一杯だ．"

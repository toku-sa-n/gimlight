{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Localization.Texts.Items
    ( herb
    , sampleBook
    , sword
    , hammer
    ) where

import           Gimlight.Localization (MultilingualText, multilingualText)

herb :: MultilingualText
herb = multilingualText "Herb" "薬草"

sampleBook :: MultilingualText
sampleBook = multilingualText "Sample book" "テスト用本"

sword :: MultilingualText
sword = multilingualText "Sword" "剣"

hammer :: MultilingualText
hammer = multilingualText "Hammer" "ハンマー"

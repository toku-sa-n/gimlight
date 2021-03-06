{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Localization.Texts.Items
    ( herb
    , sampleBook
    , sword
    , woodenArmor
    ) where

import           Gimlight.Localization (MultilingualText, multilingualText)

herb :: MultilingualText
herb = multilingualText "Herb" "薬草"

sampleBook :: MultilingualText
sampleBook = multilingualText "Sample book" "テスト用本"

sword :: MultilingualText
sword = multilingualText "Sword" "剣"

woodenArmor :: MultilingualText
woodenArmor = multilingualText "Wooden armor" "木の鎧"

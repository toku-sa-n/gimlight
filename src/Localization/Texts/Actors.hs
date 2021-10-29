{-# LANGUAGE OverloadedStrings #-}

module Localization.Texts.Actors
    ( orc
    , troll
    ) where

import           Localization (MultilingualText, multilingualText)

orc :: MultilingualText
orc = multilingualText "Orc" "オーク"

troll :: MultilingualText
troll = multilingualText "Troll" "トロール"

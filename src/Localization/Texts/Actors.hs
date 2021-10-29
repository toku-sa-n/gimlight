{-# LANGUAGE OverloadedStrings #-}

module Localization.Texts.Actors
    ( electria
    , orc
    , troll
    ) where

import           Localization (MultilingualText, multilingualText)

electria :: MultilingualText
electria = multilingualText "Electria" "エレクトリア"

orc :: MultilingualText
orc = multilingualText "Orc" "オーク"

troll :: MultilingualText
troll = multilingualText "Troll" "トロール"

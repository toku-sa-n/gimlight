{-# LANGUAGE OverloadedStrings #-}

module Localization.Texts.Actors
    ( electria
    , talkWithElectria
    , orc
    , troll
    ) where

import           Localization (MultilingualText, multilingualText)

electria :: MultilingualText
electria = multilingualText "Electria" "エレクトリア"

talkWithElectria :: MultilingualText
talkWithElectria = multilingualText "Talking test." "会話テスト．"

orc :: MultilingualText
orc = multilingualText "Orc" "オーク"

troll :: MultilingualText
troll = multilingualText "Troll" "トロール"

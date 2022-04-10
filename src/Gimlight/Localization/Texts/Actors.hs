{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Localization.Texts.Actors
    ( player
    , momo
    , yes
    , no
    , orc
    , troll
    , module Gimlight.Localization.Texts.Actors.Momo
    ) where

import           Gimlight.Localization                   (MultilingualText,
                                                          multilingualText)
import           Gimlight.Localization.Texts.Actors.Momo

player :: MultilingualText
player = multilingualText "Player" "プレイヤー"

momo :: MultilingualText
momo = multilingualText "Momo" "モモ"

yes :: MultilingualText
yes = multilingualText "Yes" "はい"

no :: MultilingualText
no = multilingualText "No" "いいえ"

orc :: MultilingualText
orc = multilingualText "Orc" "オーク"

troll :: MultilingualText
troll = multilingualText "Troll" "トロール"

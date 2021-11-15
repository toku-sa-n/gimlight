{-# LANGUAGE OverloadedStrings #-}

module Localization.Texts.Actors
    ( player
    , electria
    , talkWithElectria
    , yes
    , no
    , talkWithElectriaYes
    , talkWithElectriaNo
    , orc
    , troll
    , module Localization.Texts.Actors.Electria
    ) where

import           Localization                       (MultilingualText,
                                                     multilingualText)
import           Localization.Texts.Actors.Electria

player :: MultilingualText
player = multilingualText "Player" "プレイヤー"

electria :: MultilingualText
electria = multilingualText "Electria" "エレクトリア"

talkWithElectria :: MultilingualText
talkWithElectria = multilingualText "Talking test." "会話テスト．"

yes :: MultilingualText
yes = multilingualText "Yes" "はい"

no :: MultilingualText
no = multilingualText "No" "いいえ"

talkWithElectriaYes :: MultilingualText
talkWithElectriaYes = multilingualText "You selected yes." "はいを選択した．"

talkWithElectriaNo :: MultilingualText
talkWithElectriaNo = multilingualText "You selected no." "いいえを選択した．"

orc :: MultilingualText
orc = multilingualText "Orc" "オーク"

troll :: MultilingualText
troll = multilingualText "Troll" "トロール"

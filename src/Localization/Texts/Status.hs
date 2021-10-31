{-# LANGUAGE OverloadedStrings #-}

module Localization.Texts.Status
    ( attack
    , defence
    , levelUp
    ) where

import           Localization (MultilingualText, multilingualText)
import           TextShow     (TextShow (showt))

attack :: MultilingualText
attack = multilingualText "Attack" "攻撃"

defence :: MultilingualText
defence = multilingualText  "Defence" "防御"

levelUp :: MultilingualText -> Int -> MultilingualText
levelUp who n =
    who
    <> multilingualText " reached to level " "はレベル"
    <> multilingualText (showt n) (showt n)
    <> multilingualText "." "になった．"

{-# LANGUAGE OverloadedStrings #-}

module Localization.Texts.Scene
    ( title1
    , title2
    ) where

import           Localization (MultilingualText, multilingualText)

title1 :: MultilingualText
title1 = multilingualText "This is the English text 1." "これは日本語テキスト1です．"

title2 :: MultilingualText
title2 = multilingualText "And this is the English text 2." "そしてこれは日本語テキスト2です．"

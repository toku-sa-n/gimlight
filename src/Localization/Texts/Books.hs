{-# LANGUAGE OverloadedStrings #-}

module Localization.Texts.Books
    ( sampleBook
    ) where

import           Localization (MultilingualText, multilingualText)

sampleBook :: MultilingualText
sampleBook = multilingualText
    "This is a sample book."
    "本の実装テスト．"

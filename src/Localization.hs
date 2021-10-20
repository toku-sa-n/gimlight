module Localization
    ( MultilingualText
    , multilingualText
    , getLocalizedText
    ) where
import           Data.Text   (Text)
import           Game.Config (Config, Language (English, Japanese), getLocale)

data MultilingualText = MultilingualText
                      { en :: Text
                      , ja :: Text
                      }

multilingualText :: Text -> Text -> MultilingualText
multilingualText = MultilingualText

getLocalizedText :: MultilingualText -> Config -> Text
getLocalizedText lt cfg =
    case getLocale cfg of
        Just English  -> en lt
        Just Japanese -> ja lt
        Nothing       -> error "The language is not set."

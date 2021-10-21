{-# LANGUAGE DeriveGeneric #-}
module Localization
    ( MultilingualText
    , multilingualText
    , getLocalizedText
    ) where
import           Data.Binary  (Binary)
import           Data.Text    (Text)
import           GHC.Generics (Generic)
import           Game.Config  (Config, Language (English, Japanese), getLocale)

data MultilingualText = MultilingualText
                      { en :: Text
                      , ja :: Text
                      } deriving (Show, Eq, Ord, Generic)
instance Binary MultilingualText

multilingualText :: Text -> Text -> MultilingualText
multilingualText = MultilingualText

getLocalizedText :: Config -> MultilingualText -> Text
getLocalizedText cfg lt =
    case getLocale cfg of
        Just English  -> en lt
        Just Japanese -> ja lt
        Nothing       -> error "The language is not set."

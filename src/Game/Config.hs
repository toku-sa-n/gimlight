module Game.Config
    ( Config
    , Language(..)
    , initConfig
    , getLocale
    ) where

data Language = English | Japanese

newtype Config = Config { language :: Maybe Language }

initConfig :: Config
initConfig = Config { language = Nothing }

getLocale :: Config -> Maybe Language
getLocale Config { language = l } = l

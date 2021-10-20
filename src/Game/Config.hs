{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Config
    ( Config
    , Language(..)
    , readConfigOrDefault
    , getLocale
    ) where
import           Data.Binary  (Binary, decodeFileOrFail, encodeFile)
import           Data.Maybe   (fromMaybe)
import           GHC.Generics (Generic)

data Language = English | Japanese deriving (Generic)
instance Binary Language

newtype Config = Config { language :: Maybe Language } deriving (Generic)
instance Binary Config

readConfigOrDefault :: IO Config
readConfigOrDefault = fromMaybe initConfig <$> tryReadConfig

tryReadConfig :: IO (Maybe Config)
tryReadConfig = do
    cfg <- decodeFileOrFail configFilePath

    case cfg of
        Right x -> return $ Just x
        Left _  -> do
            encodeFile configFilePath initConfig
            return Nothing

initConfig :: Config
initConfig = Config { language = Nothing }

getLocale :: Config -> Maybe Language
getLocale Config { language = l } = l

configFilePath :: FilePath
configFilePath = "config"

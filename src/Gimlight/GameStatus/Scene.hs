{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Gimlight.GameStatus.Scene
  ( SceneHandler,
    sceneHandler,
    withoutSpeaker,
    text,
    getBackgroundImagePath,
    getCurrentScene,
    nextSceneOrFinish,
  )
where

import Control.Lens (Ixed (ix), makeLenses, view, (&), (+~), (^.), (^?))
import Data.Binary (Binary)
import Data.Text (Text)
import GHC.Generics (Generic)
import Gimlight.Data.Maybe (expectJust)
import Gimlight.GameStatus.Exploring (ExploringHandler)
import Gimlight.Localization (MultilingualText)

newtype SceneElement
  = WithoutSpeaker MultilingualText
  deriving stock (Show, Ord, Eq, Generic)

instance Binary SceneElement

data SceneHandler = SceneHandler
  { _current :: Int,
    _backgroundImage :: Text,
    _elements :: [SceneElement],
    _afterScene :: ExploringHandler
  }
  deriving (Show, Ord, Eq, Generic)

makeLenses ''SceneHandler

instance Binary SceneHandler

withoutSpeaker :: MultilingualText -> SceneElement
withoutSpeaker = WithoutSpeaker

text :: SceneElement -> MultilingualText
text (WithoutSpeaker t) = t

getBackgroundImagePath :: SceneHandler -> Text
getBackgroundImagePath = view backgroundImage

getCurrentScene :: SceneHandler -> SceneElement
getCurrentScene sh = expectJust "Index out of bounds" $ sh ^? elements . ix (sh ^. current)

sceneHandler :: Text -> [SceneElement] -> ExploringHandler -> SceneHandler
sceneHandler = SceneHandler 0

nextSceneOrFinish :: SceneHandler -> Either ExploringHandler SceneHandler
nextSceneOrFinish sh
  | nextSceneExists sh = Right $ sh & current +~ 1
  | otherwise = Left $ sh ^. afterScene

nextSceneExists :: SceneHandler -> Bool
nextSceneExists sh = sh ^. current + 1 < length (sh ^. elements)

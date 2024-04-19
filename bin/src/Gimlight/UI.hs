{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Unsafe             #-}

module Gimlight.UI
  ( start
  ) where

import           Gimlight.GameModel (GameModel, initGameModel)
import           Monomer            (AppConfig, AppEventResponse, WidgetEnv,
                                     WidgetNode, appFontDef, appTheme,
                                     darkTheme, label, startApp)
import           Prelude            (IO)

handleEvent ::
     WidgetEnv GameModel ()
  -> WidgetNode GameModel ()
  -> GameModel
  -> ()
  -> [AppEventResponse GameModel ()]
handleEvent _ _ _ _ = []

buildUI :: WidgetEnv GameModel () -> GameModel -> WidgetNode GameModel ()
buildUI _ _ = label "Hello, world!"

start :: IO ()
start = startApp initGameModel handleEvent buildUI config
  where
    config :: [AppConfig GameModel ()]
    config =
      [ appFontDef
          "Regular"
          "./zen-kakugothic/fonts/ttf/ZenKakuGothicNew-Regular.ttf"
      , appTheme darkTheme
      ]

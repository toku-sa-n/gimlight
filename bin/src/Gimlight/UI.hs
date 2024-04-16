{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Unsafe             #-}

module Gimlight.UI
  ( start
  ) where

import           Gimlight.GameModel (GameModel, initGameModel)
import           Monomer            (AppEventResponse, WidgetEnv, WidgetNode,
                                     label, startApp)
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
    config :: [a]
    config = []

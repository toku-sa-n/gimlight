{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Unsafe                   #-}

module Main
  ( main
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

main :: IO ()
main = startApp initGameModel handleEvent buildUI config
  where
    config :: [a]
    config = []

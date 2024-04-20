{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Unsafe             #-}

module Gimlight.UI
  ( start
  ) where

import           Data.Text          (pack)
import           Gimlight.GameModel (GameModel, getCount, increment,
                                     initGameModel)
import           Monomer            (AppConfig, AppEventResponse,
                                     EventResponse (Model), WidgetEnv,
                                     WidgetNode, appFontDef, appTheme,
                                     darkTheme, keystroke, label, startApp)
import           Prelude            (IO, Show (show), ($), (<>))

handleEvent ::
     WidgetEnv GameModel ()
  -> WidgetNode GameModel ()
  -> GameModel
  -> ()
  -> [AppEventResponse GameModel ()]
handleEvent _ _ model () = [Model $ increment model]

buildUI :: WidgetEnv GameModel () -> GameModel -> WidgetNode GameModel ()
buildUI _ model =
  keystroke [("Enter", ())] $
  label $
  "You pressed the Enter key " <> pack (show (getCount model)) <> " times"

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

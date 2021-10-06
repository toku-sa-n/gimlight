module UI.Types
    ( Tick(..)
    , Name
    , AppEvent(..)
    ) where

data Tick = Tick

type Name = ()

data AppEvent = AppInit | AppKeyboardInput deriving (Eq, Show)

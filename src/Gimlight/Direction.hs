{-# LANGUAGE DeriveGeneric #-}

module Gimlight.Direction
    ( Direction(..)
    , allDirections
    , fromUnitVector
    , toUnitVector
    ) where

import           GHC.Generics     (Generic)
import           GHC.Stack        (HasCallStack)
import           Gimlight.Prelude
import           Linear           (V2 (V2))

data Direction
    = North
    | South
    | East
    | West
    | NorthEast
    | NorthWest
    | SouthEast
    | SouthWest
    deriving (Show, Ord, Eq, Generic, Enum, Bounded)

allDirections :: [Direction]
allDirections = [minBound ..]

fromUnitVector :: HasCallStack => V2 Int -> Direction
fromUnitVector (V2 0 (-1))    = North
fromUnitVector (V2 0 1)       = South
fromUnitVector (V2 1 0)       = East
fromUnitVector (V2 (-1) 0)    = West
fromUnitVector (V2 1 (-1))    = NorthEast
fromUnitVector (V2 (-1) (-1)) = NorthWest
fromUnitVector (V2 1 1)       = SouthEast
fromUnitVector (V2 (-1) 1)    = SouthWest
fromUnitVector _              = error "Not a unit vector."

toUnitVector :: Direction -> V2 Int
toUnitVector North     = V2 0 (-1)
toUnitVector South     = V2 0 1
toUnitVector East      = V2 1 0
toUnitVector West      = V2 (-1) 0
toUnitVector NorthEast = V2 1 (-1)
toUnitVector NorthWest = V2 (-1) (-1)
toUnitVector SouthEast = V2 1 1
toUnitVector SouthWest = V2 (-1) 1

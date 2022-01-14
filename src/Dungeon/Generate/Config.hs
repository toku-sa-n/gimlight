module Dungeon.Generate.Config
    ( Config(..)
    , config
    ) where

import           Linear.V2      (V2 (V2))
import           UI.Draw.Config (tileColumns, tileRows)

data Config =
    Config
        { numOfFloors :: Int
        , maxRooms    :: Int
        , roomMinSize :: Int
        , roomMaxSize :: Int
        , mapSize     :: V2 Int
        }

config :: Int -> Int -> Int -> Int -> V2 Int -> Config
config nf mr rmin rmax ms@(V2 mw mh)
    | mw < tileColumns =
        error $
        "Map width is expected to be larger than or equal to " ++
        show tileColumns ++ " but the actual value is " ++ show mw ++ "."
    | mh < tileRows =
        error $
        "Map height is expected to be larger than or equal to " ++
        show tileRows ++ " but the actual value is " ++ show mh ++ "."
    | otherwise = Config nf mr rmin rmax ms

module Dungeon.Generate.Config
    ( Config(..)
    , config
    , mapWidthIsTooSmall
    , mapHeightIsTooSmall
    , roomMinIsLargerThanRoomMax
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
    | mw < tileColumns = error $ mapWidthIsTooSmall mw
    | mh < tileRows = error $ mapHeightIsTooSmall mh
    | rmin > rmax = error $ roomMinIsLargerThanRoomMax rmin rmax
    | otherwise = Config nf mr rmin rmax ms

mapWidthIsTooSmall :: Int -> String
mapWidthIsTooSmall w =
    "Map width is expected to be larger than or equal to " ++
    show tileColumns ++ " but the actual value is " ++ show w ++ "."

mapHeightIsTooSmall :: Int -> String
mapHeightIsTooSmall h =
    "Map height is expected to be larger than or equal to " ++
    show tileColumns ++ " but the actual value is " ++ show h ++ "."

roomMinIsLargerThanRoomMax :: Int -> Int -> String
roomMinIsLargerThanRoomMax rmin rmax =
    "The room minimum size " ++
    show rmin ++ " is larger than the room maximum size " ++ show rmax ++ "."

module UI.Graphics
    ( Graphics
    , graphics
    , getMapTiles
    ) where

import           Data.Map             (empty)
import           UI.Graphics.MapTiles (MapTiles)
import qualified UI.Graphics.MapTiles as MapTiles

newtype Graphics =
    Graphics MapTiles
    deriving (Eq)

graphics :: IO (Maybe Graphics)
graphics = do
    mt <- MapTiles.addTileFile "maps/tiles.json" "images/map_tiles.png" empty
    return $ Graphics <$> mt

getMapTiles :: Graphics -> MapTiles
getMapTiles (Graphics mt) = mt

module UI.Draw.Map
    ( mapGrid
    ) where
import           Data.Text (pack)
import           Monomer   (CmbAlignLeft (alignLeft), CmbHeight (height),
                            CmbStyleBasic (styleBasic), CmbWidth (width),
                            WidgetEvent, WidgetModel, WidgetNode, box_, hgrid,
                            image, vgrid)

mapGrid :: WidgetModel s => WidgetEvent e => WidgetNode s e
mapGrid = box_ [alignLeft] $ vgrid (replicate tileRows rows) `styleBasic` styles
    where rows = hgrid $ replicate tileColumns $ image $ pack "kabe.png"
          styles = [ width $ fromIntegral mapWidth
                   , height $ fromIntegral mapHeight]
          tileRows = mapHeight `div` tileHeight
          tileColumns = mapWidth `div` tileWidth

mapWidth, mapHeight :: Int
mapWidth = 768
mapHeight = 576

tileWidth, tileHeight :: Int
tileWidth = 48
tileHeight = 48

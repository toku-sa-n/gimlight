module UI.Draw.Map
    ( mapGrid
    ) where
import           Data.Text (pack)
import           Monomer   (CmbAlignLeft (alignLeft), CmbHeight (height),
                            CmbPaddingL (paddingL), CmbPaddingT (paddingT),
                            CmbStyleBasic (styleBasic), CmbWidth (width),
                            WidgetEvent, WidgetModel, WidgetNode, box_, hgrid,
                            image, vgrid, zstack)

mapGrid :: (WidgetModel s, WidgetEvent e) => WidgetNode s e
mapGrid = zstack [ mapTiles
                 , image (pack "images/player.png") `styleBasic` [paddingL 48, paddingT 48]
                 ] `styleBasic` [ width $ fromIntegral mapWidth
                                , height $ fromIntegral mapHeight
                                ]

mapTiles :: WidgetModel s => WidgetEvent e => WidgetNode s e
mapTiles = box_ [alignLeft] $ vgrid (replicate tileRows rows) `styleBasic` styles
    where rows = hgrid $ replicate tileColumns $ image $ pack "images/grass.png"
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

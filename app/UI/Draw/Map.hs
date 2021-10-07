module UI.Draw.Map
    ( mapGrid
    ) where
import           Control.Lens  ((^.))
import           Data.Text     (pack)
import           Dungeon.Types (entities, imagePath, position)
import           Engine        (Engine (PlayerIsExploring))
import           Linear.V2     (_x, _y)
import           Monomer       (CmbAlignLeft (alignLeft), CmbHeight (height),
                                CmbPaddingL (paddingL), CmbPaddingT (paddingT),
                                CmbStyleBasic (styleBasic), CmbWidth (width),
                                WidgetEvent, WidgetModel, WidgetNode, box_,
                                hgrid, image, vgrid, zstack)

mapGrid :: (WidgetModel s, WidgetEvent e) => Engine -> WidgetNode s e
mapGrid engine = zstack (mapTiles:mapEntities engine) `styleBasic` [ width $ fromIntegral mapWidth
                                                                   , height $ fromIntegral mapHeight
                                                                   ]

mapTiles :: (WidgetModel s, WidgetEvent e) => WidgetNode s e
mapTiles = box_ [alignLeft] $ vgrid (replicate tileRows rows) `styleBasic` styles
    where rows = hgrid $ replicate tileColumns $ image $ pack "images/grass.png"
          styles = [ width $ fromIntegral mapWidth
                   , height $ fromIntegral mapHeight]

mapEntities :: (WidgetModel s, WidgetEvent e) => Engine -> [WidgetNode s e]
mapEntities (PlayerIsExploring d _ _) = map (\e -> image (pack $ e ^. imagePath) `styleBasic` [paddingL $ fromIntegral $ e ^. (position . _x) * tileWidth, paddingT $ fromIntegral $ mapHeight - ((e ^. (position . _y) + 1) * tileHeight)]) $ d ^. entities
mapEntities _                         = undefined

mapWidth, mapHeight :: Int
mapWidth = tileWidth * tileColumns
mapHeight = tileHeight * tileRows

tileWidth, tileHeight :: Int
tileWidth = 48
tileHeight = 48

tileColumns, tileRows :: Int
tileColumns = 17
tileRows = 9

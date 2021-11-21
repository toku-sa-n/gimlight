{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Exploring
    ( drawExploring
    ) where

import           Actor                           (getCurrentExperiencePoint,
                                                  getDefence,
                                                  getExperiencePointForNextLevel,
                                                  getHp, getLevel, getMaxHp,
                                                  getPower, walkingImagePath)
import qualified Actor                           as A
import           Codec.Picture                   (Image (imageData),
                                                  PixelRGBA8 (PixelRGBA8),
                                                  pixelMap)
import           Control.Applicative             (ZipList (ZipList, getZipList))
import           Control.Lens                    ((^.))
import           Control.Monad                   (guard)
import           Coord                           (Coord)
import           Data.Array                      ((!))
import           Data.Maybe                      (mapMaybe)
import           Data.Vector.Split               (chunksOf)
import qualified Data.Vector.Storable            as V
import           Data.Vector.Storable.ByteString (vectorToByteString)
import           Dungeon                         (Dungeon, actors, explored,
                                                  items, mapWidthAndHeight,
                                                  playerPosition, tileMap,
                                                  visible)
import           GameConfig                      (GameConfig)
import           GameStatus.Exploring            (ExploringHandler,
                                                  getCurrentDungeon,
                                                  getMessageLog, getPlayerActor)
import qualified Item                            as I
import           Linear.V2                       (V2 (V2), _x, _y)
import           Localization                    (getLocalizedText)
import qualified Localization.Texts              as T
import           Monomer                         (CmbAlignLeft (alignLeft),
                                                  CmbHeight (height),
                                                  CmbMultiline (multiline),
                                                  CmbPaddingL (paddingL),
                                                  CmbPaddingT (paddingT),
                                                  CmbStyleBasic (styleBasic),
                                                  CmbWidth (width), Size (Size),
                                                  box_, hstack, image, imageMem,
                                                  label, label_, vstack, zstack)
import qualified Monomer.Lens                    as L
import           TextShow                        (TextShow (showt))
import           UI.Draw.Config                  (logRows, tileColumns,
                                                  tileHeight, tileRows,
                                                  tileWidth, windowWidth)
import           UI.Draw.KeyEvent                (withKeyEvents)
import           UI.Graphics.MapTiles            (MapTiles)
import           UI.Types                        (GameWidgetEnv, GameWidgetNode)

drawExploring ::
       GameWidgetEnv
    -> MapTiles
    -> ExploringHandler
    -> GameConfig
    -> GameWidgetNode
drawExploring wenv tileGraphics eh c =
    withKeyEvents $ vstack [statusAndMapGrid, messageLogArea eh c]
  where
    statusAndMapGrid =
        hstack
            [ mapGrid wenv tileGraphics eh
            , statusGrid eh c `styleBasic`
              [width $ fromIntegral $ windowWidth - tileWidth * tileColumns]
            ]

messageLogArea :: ExploringHandler -> GameConfig -> GameWidgetNode
messageLogArea eh c =
    vstack $
    fmap (\x -> label_ (getLocalizedText c x) [multiline]) $
    take logRows $ getMessageLog eh

mapGrid :: GameWidgetEnv -> MapTiles -> ExploringHandler -> GameWidgetNode
mapGrid wenv tileGraphics eh =
    zstack (mapTiles wenv tileGraphics eh : (mapItems eh ++ mapActors eh)) `styleBasic`
    [ width $ fromIntegral mapDrawingWidth
    , height $ fromIntegral mapDrawingHeight
    ]

statusGrid :: ExploringHandler -> GameConfig -> GameWidgetNode
statusGrid eh c =
    vstack $
    maybe
        []
        (\x ->
             [ label "Player"
             , label $ lvl <> ": " <> showt (getLevel x)
             , label $
               experience <>
               ": " <>
               showt (getCurrentExperiencePoint x) <>
               " / " <> showt (getExperiencePointForNextLevel x)
             , label $ "HP: " <> showt (getHp x) <> " / " <> showt (getMaxHp x)
             , label $ atk <> ": " <> showt (getPower x)
             , label $ def <> ": " <> showt (getDefence x)
             ]) $
    getPlayerActor eh
  where
    lvl = getLocalizedText c T.level
    experience = getLocalizedText c T.experience
    atk = getLocalizedText c T.attack
    def = getLocalizedText c T.defence

mapTiles :: GameWidgetEnv -> MapTiles -> ExploringHandler -> GameWidgetNode
mapTiles wenv tileGraphics eh =
    box_ [alignLeft] $ imageMem imageName rows mapSize `styleBasic` styles
  where
    imageName = showt (wenv ^. L.timestamp)
    rows =
        vectorToByteString $
        V.concat [row y | y <- [topLeftCoordY .. topLeftCoordY + tileRows - 1]]
    row y =
        V.concat $
        getZipList $
        foldl1
            (\acc x -> (V.++) <$> acc <*> x)
            [ ZipList $ imageAt $ V2 x y
            | x <- [topLeftCoordX .. topLeftCoordX + tileColumns - 1]
            ]
    imageAt c =
        chunksOf (tileWidth * 4) $ -- `(*4)` for R, G, B, and A bytes.
        imageData $
        pixelMap (applyOpacity c) $ tileGraphics ! ((d ^. tileMap) ! c)
    applyOpacity c (PixelRGBA8 r g b a)
        | isVisible c = PixelRGBA8 r g b a
        | isExplored c = PixelRGBA8 (r `div` 2) (g `div` 2) (b `div` 2) a
        | otherwise = PixelRGBA8 0 0 0 0xff
    isVisible c = (d ^. visible) ! c
    isExplored c = (d ^. explored) ! c
    d = getCurrentDungeon eh
    V2 topLeftCoordX topLeftCoordY = topLeftCoord d
    styles =
        [ width $ fromIntegral mapDrawingWidth
        , height $ fromIntegral mapDrawingHeight
        ]

mapItems :: ExploringHandler -> [GameWidgetNode]
mapItems eh = mapMaybe itemToImage $ d ^. items
  where
    itemToImage item =
        guard (isItemDrawed item) >>
        return (image (I.getIconImagePath item) `styleBasic` style item)
    isItemDrawed item =
        let pos = itemPositionOnDisplay item
            isVisible = (d ^. visible) ! I.getPosition item
         in V2 0 0 <= pos && pos <= bottomRightCoord d && isVisible
    d = getCurrentDungeon eh
    leftPadding item =
        fromIntegral $ itemPositionOnDisplay item ^. _x * tileWidth
    topPadding item =
        fromIntegral $ itemPositionOnDisplay item ^. _y * tileHeight
    style item = [paddingL $ leftPadding item, paddingT $ topPadding item]
    itemPositionOnDisplay item = I.getPosition item - topLeftCoord d

mapActors :: ExploringHandler -> [GameWidgetNode]
mapActors eh = mapMaybe actorToImage $ d ^. actors
  where
    d = getCurrentDungeon eh
    leftPadding actor =
        fromIntegral $ actorPositionOnDisplay actor ^. _x * tileWidth
    topPadding actor =
        fromIntegral $ (actorPositionOnDisplay actor ^. _y) * tileHeight
    style actor = [paddingL $ leftPadding actor, paddingT $ topPadding actor]
    actorPositionOnDisplay actor = actor ^. A.position - topLeftCoord d
    isActorDrawed actor =
        let pos = actorPositionOnDisplay actor
            isVisible = (d ^. visible) ! (actor ^. A.position)
         in V2 0 0 <= pos && pos <= bottomRightCoord d && isVisible
    actorToImage actor =
        guard (isActorDrawed actor) >>
        return (image (actor ^. walkingImagePath) `styleBasic` style actor)

topLeftCoord :: Dungeon -> Coord
topLeftCoord d = V2 x y
  where
    V2 unadjustedX unadjestedY =
        maybe
            (V2 0 0)
            (\pos -> pos - V2 (tileColumns `div` 2) (tileRows `div` 2))
            (playerPosition d)
    V2 maxX maxY = mapWidthAndHeight d - V2 tileColumns tileRows
    x = max 0 $ min maxX unadjustedX
    y = max 0 $ min maxY unadjestedY

bottomRightCoord :: Dungeon -> Coord
bottomRightCoord d = topLeftCoord d + mapWidthAndHeight d - V2 1 1

mapSize :: Size
mapSize = Size (fromIntegral mapDrawingWidth) (fromIntegral mapDrawingHeight)

mapDrawingWidth :: Int
mapDrawingWidth = tileWidth * tileColumns

mapDrawingHeight :: Int
mapDrawingHeight = tileHeight * tileRows

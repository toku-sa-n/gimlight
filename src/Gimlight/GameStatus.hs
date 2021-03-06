{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Gimlight.GameStatus
    ( GameStatus(..)
    , newGameStatus
    ) where

import           Control.Lens                          ()
import           Control.Monad.State                   (StateT (runStateT),
                                                        evalState, evalStateT)
import           Data.Foldable                         (foldlM)
import           Data.List.NonEmpty                    (fromList)
import           Data.Map                              (empty)
import           Data.Tree                             (Tree (Node, rootLabel, subForest))
import           GHC.Generics                          (Generic)
import           Gimlight.Actor.WalkingImages          (readIntegratedImagesRecursive)
import           Gimlight.Data.Maybe                   (expectJust)
import           Gimlight.Dungeon                      (addAscendingAndDescendingStiars,
                                                        addDescendingStairs)
import           Gimlight.Dungeon.Init                 (initDungeon)
import           Gimlight.Dungeon.Map.Tile.JSONReader  (addTileFile)
import           Gimlight.Dungeon.Predefined.BatsCave  (batsDungeon)
import           Gimlight.Dungeon.Predefined.GlobalMap (globalMap)
import           Gimlight.Dungeon.Stairs               (StairsPair (StairsPair))
import           Gimlight.GameStatus.Exploring         (ExploringHandler,
                                                        exploringHandler)
import           Gimlight.GameStatus.ReadingBook       (ReadingBookHandler)
import           Gimlight.GameStatus.Scene             (SceneHandler,
                                                        sceneHandler,
                                                        withoutSpeaker)
import           Gimlight.GameStatus.SelectingItem     (SelectingItemHandler)
import           Gimlight.GameStatus.Talking           (TalkingHandler)
import           Gimlight.IndexGenerator               (generator)
import qualified Gimlight.Localization.Texts.Scene     as T (title, welcome)
import qualified Gimlight.Log                          as L
import           Gimlight.Prelude
import           Gimlight.Quest                        (questCollection)
import           Gimlight.System.Directory             (getFilesRecursive)
import           Gimlight.TreeZipper                   (appendTree, goDownBy,
                                                        treeZipper)
import           Linear.V2                             (V2 (V2))
import           System.Random                         (getStdGen)

data GameStatus
    = Exploring ExploringHandler
    | Talking TalkingHandler
    | Scene SceneHandler
    | SelectingItem SelectingItemHandler
    | ReadingBook ReadingBookHandler
    | Title
    | GameOver
    | SelectingLocale
    deriving (Eq, Generic)

newGameStatus :: IO GameStatus
newGameStatus = do
    g <- getStdGen
    tc <- getFilesRecursive "tiles/" >>= foldlM (flip addTileFile) empty
    walkingImages <- readIntegratedImagesRecursive "images/walking_pictures/"
    gm <- globalMap
    (beaeve, ig) <- runStateT (initDungeon tc) generator
    let (bats, stairsPosition) =
            flip evalState g $ evalStateT (batsDungeon tc) ig
        (gmWithBatsStairs, batsRootMapWithParentMap) =
            addAscendingAndDescendingStiars
                (StairsPair (V2 7 5) stairsPosition)
                (gm, rootLabel bats)
        batsTreeWithParentMap = bats {rootLabel = batsRootMapWithParentMap}
        (initGm, beaeveWithParentMap) =
            addDescendingStairs
                (StairsPair (V2 4 6) (V2 7 0))
                (gmWithBatsStairs, beaeve)
        dungeonTree =
            Node
                { rootLabel = initGm
                , subForest =
                      [Node {rootLabel = beaeveWithParentMap, subForest = []}]
                }
        zipper = appendTree batsTreeWithParentMap $ treeZipper dungeonTree
        initZipper =
            expectJust "Unreachable." (goDownBy (== beaeveWithParentMap) zipper)
        initExploring =
            exploringHandler
                initZipper
                (foldr (L.addMessage . L.message) L.emptyLog [T.welcome])
                questCollection
                tc
                walkingImages
    return . Scene $
        sceneHandler
            "images/game_opening.png"
            (fromList $ fmap withoutSpeaker T.title)
            initExploring

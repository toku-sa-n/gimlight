{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Gimlight.TreeZipper
    ( TreeZipper
    , treeZipper
    , goToRootAndGetTree
    , focused
    , goToRoot
    , goUp
    , goDownBy
    , appendNode
    , appendTree
    ) where

import           Control.Lens     (Lens', lens)
import           Data.Foldable    (find)
import           Data.Tree        (Tree (Node, rootLabel, subForest))
import           GHC.Generics     (Generic)
import           Gimlight.Prelude

data TreeCrumb a =
    TreeCrumb a [Tree a]
    deriving (Show, Ord, Eq, Generic)

type TreeZipper a = (Tree a, [TreeCrumb a])

treeZipper :: Tree a -> TreeZipper a
treeZipper t = (t, [])

goToRootAndGetTree :: TreeZipper a -> Tree a
goToRootAndGetTree z = fst $ goToRoot z

focused :: Lens' (TreeZipper a) a
focused = lens getter setter
  where
    getter (t, _) = rootLabel t
    setter (Node _ ts, c) t = (Node t ts, c)

goToRoot :: TreeZipper a -> TreeZipper a
goToRoot z = maybe z goToRoot (goUp z)

goUp :: TreeZipper a -> Maybe (TreeZipper a)
goUp (Node {rootLabel = r, subForest = fs}, TreeCrumb newRootLabel newSubForest:bs) =
    Just
        ( Node {rootLabel = newRootLabel, subForest = Node r fs : newSubForest}
        , bs)
goUp (_, []) = Nothing

goDownBy :: (a -> Bool) -> TreeZipper a -> Maybe (TreeZipper a)
goDownBy f (Node {rootLabel = r, subForest = ts}, bs) =
    (, TreeCrumb r ts : bs) <$> find (f . rootLabel) ts

appendNode :: a -> TreeZipper a -> TreeZipper a
appendNode n (z@Node {subForest = ts}, bs) = (z {subForest = newNode : ts}, bs)
  where
    newNode = Node {rootLabel = n, subForest = []}

appendTree :: Tree a -> TreeZipper a -> TreeZipper a
appendTree tree (z@Node {subForest = ts}, bs) = (z {subForest = tree : ts}, bs)

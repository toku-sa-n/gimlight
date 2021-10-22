{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module TreeZipper
    ( TreeZipper
    , treeZipper
    , getFocused
    , goDownBy
    ) where

import           Data.Foldable (find)
import           Data.Tree     (Tree (Node, rootLabel, subForest))
import           GHC.Generics  (Generic)

data TreeCrumb a = TreeCrumb a [Tree a] deriving (Show, Ord, Eq, Generic)

type TreeZipper a = (Tree a, [TreeCrumb a])

treeZipper :: Tree a -> TreeZipper a
treeZipper t = (t, [])

getFocused :: TreeZipper a -> a
getFocused (t, _) = rootLabel t

goDownBy :: (a -> Bool) -> TreeZipper a -> Maybe (TreeZipper a)
goDownBy f (Node { rootLabel = r, subForest = ts }, bs) =
    (, TreeCrumb r ts:bs) <$> find (f . rootLabel)  ts

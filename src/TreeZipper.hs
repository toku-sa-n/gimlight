module TreeZipper
    ( TreeZipper
    , treeZipper
    ) where

import           Data.Tree (Tree)

data TreeCrumb a = TreeCrumb a [Tree a]

type TreeZipper a = (Tree a, [TreeCrumb a])

treeZipper :: Tree a -> TreeZipper a
treeZipper t = (t, [])

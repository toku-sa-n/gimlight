module Gimlight.Data.List
    ( filterAll
    ) where

filterAll :: [a -> Bool] -> [a] -> [a]
filterAll preds xs = foldr filter xs preds

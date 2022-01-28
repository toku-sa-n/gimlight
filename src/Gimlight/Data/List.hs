module Gimlight.Data.List
    ( intercalateIncludingHeadTail
    , makeTable
    ) where

import           Data.List (intercalate)

intercalateIncludingHeadTail :: [a] -> [[a]] -> [a]
intercalateIncludingHeadTail x xs = x ++ intercalate x xs ++ x

makeTable :: [[String]] -> String
makeTable rows = insertSeps paddedRows
  where
    paddedRows = fmap (fmap addPad) rows
    insertSeps = insertHseps . fmap insertVseps
    insertHseps = init . intercalateIncludingHeadTail (hsep ++ "\n")
    insertVseps = (++ "\n") . intercalateIncludingHeadTail "|"
    hsep = intercalateIncludingHeadTail "+" $ replicate numCols cellHsep
    cellHsep = replicate cellWidth '-'
    cellWidth = maximum $ fmap (maximum . fmap length) rows
    numCols = maximum $ fmap length rows
    addPad s = s ++ replicate (cellWidth - length s) ' '

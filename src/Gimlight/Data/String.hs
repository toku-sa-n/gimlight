module Gimlight.Data.String
    ( adjustLength
    ) where

adjustLength :: Int -> String -> String
adjustLength n s = s ++ replicate (n - length s) ' '

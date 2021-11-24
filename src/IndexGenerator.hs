module IndexGenerator
    ( IndexGenerator
    , Index
    , generate
    ) where

type IndexGenerator = Int

type Index = Int

-- I assume that the number of actors in the game that exist in the same
-- time will not exceed 0xffff_ffff.
generate :: IndexGenerator -> (Index, IndexGenerator)
generate n = (n, n + 1)

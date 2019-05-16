module Util
  ( parListSplitFold
  , parMapChunk
  ) where

import Control.DeepSeq (NFData)
import Control.Parallel.Strategies (parList, parListChunk, rdeepseq, using)
import Data.List.Split (chunksOf)

parMapChunk :: (NFData b) => Int -> (a -> b) -> [a] -> [b]
parMapChunk s f = (`using` parListChunk s rdeepseq) . map f

parListSplitFold :: (NFData b) => Int -> ([a] -> b) -> ([b] -> b) -> [a] -> b
parListSplitFold s m r l
  | s <= 1    = r $ ((`using` parList rdeepseq) . (pure . m)) l
  | otherwise = r $ ((`using` parList rdeepseq) . map m) (chunksOf s l)

module Fragem.Metrics.BeatMetric where

import           Control.Arrow ((&&&))
import           Data.Function (on)
import qualified Data.Map              as M
import qualified Data.List             as L

import Fragem.Syntax
import Fragem.Metrics.Base

type Granularity = Int

-- |Assigns different weights depending on the strong
--  beats of a time signature.
--
--  For instance, if the midi is at 120 ticks-per-beat
--  and time signature is 4 by 4:
--    - we assign weights 4, 1, 2, 1 for ticks 0, 120, 240, 360.
-- 
metricBeat :: TimeSig -> Int -> [Measure] -> Metric
metricBeat ts tpb ms
  = go . preprocess . map measureNotes $ ms
  where
    tphalf = tpb `div` 2
    
    beatWeights :: TimeSig -> [Int]
    beatWeights (TimeSig 4 4) = [5,2,3,2,4,2,3,2]
    beatWeights (TimeSig 3 4) = [4,2,3,2,3,2]
    beatWeights (TimeSig 2 4) = [4,2,3,2]
    beatWeights ts = error $ "beatWeights: not yet implemented for " ++ show ts

    cycleN 0 _ = []
    cycleN n l = l ++ cycleN (n-1) l

    go :: (Metric , Ticks) -> Metric
    go (m , init)
      = L.foldl' aux m (zip (map (init+) [0 , tphalf ..])
                            (cycleN (length ms) (beatWeights ts)))
      where
        aux m (tick , weight)
          = M.adjust (const weight) tick m
     
    preprocess :: [[Note]] -> (Metric , Ticks)
    preprocess nns = let ns = concat nns
                      in ( M.fromList (map (\n -> (noteDelay n , 1)) ns)
                         , noteDelay $ head ns
                         )


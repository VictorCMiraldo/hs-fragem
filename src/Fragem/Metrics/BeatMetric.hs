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

{-
metricTest1 :: [Measure]
metricTest1 = [ Measure [ Note i          whole       50
                        , Note (i + wh)   eigth       47
                        , Note (i + ww)   half        47
                        , Note (i + wwh)  half        45
                        , Note (i + wwwh) (half + wh) 50
                        ]
              , Measure [ Note (i + 4*w + wh) eigth 47
                        , Note (i + 6*w)      half  47
                        ]
              ]
  where
    tpb = 60
    i = 0
    wh = whole + half
    whole = tpb
    w     = whole
    ww    = 2*w
    wwh   = w + wh
    wwwh  = ww + wh
    half  = whole `div` 2
    eigth = half `div` 2
-}

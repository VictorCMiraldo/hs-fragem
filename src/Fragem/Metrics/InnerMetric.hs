module Fragem.Metrics.InnerMetric where

import           Control.Arrow ((&&&))
import           Data.Function (on)
import qualified Data.Map              as M
import qualified Data.List             as L

import Fragem.Syntax
import Fragem.Metrics.Base


-- |Performs the metric weight analisys
--  on a section. The parameter is the number of measures
--  to consider as a "block"
--
--  > let measures = ----..-.--,--..--|-------.--......
--  >  in metricWieght 2 [ measures , ... ]
--
--  Should return something in the lines of:
--
--  > OR = ----..-.--,--..--|-------.--......
--  > M1 = X.....X.X. X...X. .........X.......
--  > M2 = ......X.X. X..... ..................
--  > M3 = ......X... X...X. ..................
--  > M4 = ......X... ....X. .........X........
--               ^                    ^
--               t                    u
--
--  This will provide our notion of 'zoom'. Here, at zoom level 'n'
--  we only look at the notes with weight at least 'max - n'.
--  In this example; at zoom level 0, we look only at note at tick 't'
--  in zoom in a bit further additionally uncovers the notes with weight 3
--  and so on and so forth.
--
--  > metricWeight metricTest1
--  >  == fromList [(42,1),(132,4),(162,2),(192,3),(252,3),(372,2),(402,1)]
--
metricInner :: [Measure] -> Metric
metricInner
  = go . preprocess . map measureNotes
  where
    -- Puts all the onsets in a map and returs
    -- a pairwise list of notes to be further processed.
    preprocess :: [[Note]] -> (Metric , [(Note,Note)])
    preprocess nns = let ns = concat nns
                      in ( M.fromList (map (\n -> (noteDelay n , 1)) ns)
                         , pairwise ns)

    -- pipelining of types
    go :: (Metric , [(Note , Note)]) -> Metric
    go = uncurry (L.foldl' (\s (ni , nj) -> walkOnsets ni nj s))

    -- Given two notes; if they make a pulse (ie. there exists
    -- yet a thirs note with the same distance) we walk on
    -- that pulse increasing the notes weights.
    walkOnsets :: Note -> Note -> Metric -> Metric
    walkOnsets ni nj m
      = let candidate = noteDelay ni + 2*dist
         in if not (candidate `M.member` m)
            then m
            else streakInc (noteDelay ni) 0 m
      where
        dist = noteDelay nj - noteDelay ni

        streakInc dni ctr m
          = let m' = M.adjust (+1) (dni + ctr*dist) m
             in if (dni + (ctr+1)*dist) `M.member` m'
                then streakInc dni (ctr+1) m'
                else m'
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
    i = 42
    wh = whole + half
    whole = tpb
    w     = whole
    ww    = 2*w
    wwh   = w + wh
    wwwh  = ww + wh
    half  = whole `div` 2
    eigth = half `div` 2
    wholeDot = whole + half
-}

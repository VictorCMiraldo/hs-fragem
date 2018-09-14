
module Fragem.Metrics.Base where

import qualified Data.Map as M
import qualified Data.List as L

import           Fragem.Syntax

-- * general utilities

pairwise :: [a] -> [(a , a)]
pairwise [] = []
pairwise (n:ns) = [(n , k) | k <- ns] ++ pairwise ns

groupsOf :: Int -> [a] -> [[a]]
groupsOf n ls
  | length ls < n = []
  | otherwise     = take n ls : groupsOf n (drop n ls)


-- |A 'metric' is a way of zoomin in or out a piece of music
-- for the purpose of computing its Hausdorff dimension.
--
-- Essentially, we assign a weight to each tick and at zoom
-- level 'n', we only look at ticks with weight at least 'n'.
type Metric = M.Map Ticks Int

-- |Given a metric over some measures; we will produce the
--  /zoom/ levels based on that.
--
--  PRECONDITION: All noteDelays are elements
--                of the metric
zoomAt :: Int -> Metric -> [Measure] -> [Measure]
zoomAt n metric = map (Measure . filter go . measureNotes)
  where
    metricMax = maximum (metricLevels metric)

    -- Look into https://github.com/VictorCMiraldo/hs-fragem/issues/15
    -- Metric levels are inverted inside the Fragem.Metrics folder:
    --
    -- Zoom level 0 means every note is present, the
    -- greater the number, means less notes are present.
    --
    -- It is much more intuitive to have a bigger number
    -- mean potentially more notes
    trZoomLevel n
      | metricMax - n < 1 = 1
      | otherwise         = metricMax - n
     
    go :: Note -> Bool
    go nt = case M.lookup (noteDelay nt) metric of
      Just k  -> k >= trZoomLevel n
      Nothing -> False

-- |Returns the available zoom-levels in a metric
metricLevels :: Metric -> [Int]
metricLevels = L.nub . map snd . M.toList

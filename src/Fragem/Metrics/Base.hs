
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
    go :: Note -> Bool
    go nt | Just k <- M.lookup (noteDelay nt) metric
         = k >= n

-- |Returns the available zoom-levels in a metric
metricLevels :: Metric -> [Int]
metricLevels = L.nub . map snd . M.toList

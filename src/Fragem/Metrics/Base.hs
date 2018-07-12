
module Fragem.Metrics.Base where

import qualified Data.Map as M

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

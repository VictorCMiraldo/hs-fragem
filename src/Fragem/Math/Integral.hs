{-# LANGUAGE TupleSections #-}
-- Provides an interface to compute a n-dimensional integrals.
module Fragem.Math.Integral where

import Data.List
import Control.Arrow ((***))
import Debug.Trace

type Point = (Double , Double)

type Line = [Point]

pointX , pointY :: Point -> Double
pointX = fst
pointY = snd

(.+.) :: Point -> Point -> Point
(ax , ay) .+. (bx , by) = (ax + bx , ay + by)

-- * Auxiliary Monsters

-- | Pythagoras!
distance :: Point -> Point -> Double
distance (px , py) (cx , cy)
  = sqrt ((px - cx) ^ 2 + (py - cy) ^ 2) 

-- * Gemoetric Constructions

-- |Returns the length of a line segment.
lengthOfSegment :: Line -> Double
lengthOfSegment points
  | length points < 2 = 0
  | (p:ps) <- points  = go p ps
  where
    -- we need to keep track of the previous point
    -- to compute the hypotenuses:
    --
    --   +----+
    --         \
    --          \  
    --           +-----+
    --        ^^^ 
    --        
    --
    go :: Point -> Line -> Double
    go _  [] = 0                                                  
    go p (q : rest) = distance p q + go q rest

-- |Returns the area under a segment
--
--  PRECONDITION: the x coordinates are monotonically
--                increasing in the list.
areaOfSegment :: Line -> Double
areaOfSegment points
  | length points < 2 = 0
  -- TODO, enforce this precond here instead of requiring it.
  | (p:ps) <- points  = go p ps
  where
    go :: Point -> Line -> Double
    go _ [] = 0
    go p@(px , py) (q@(qx , qy) : rest)
      = let deltaX = qx - px
            sqA    = deltaX * min py qy
            trA    = deltaX * abs (py - qy) / 2
         in sqA + trA + go q rest

-- | Interpolates a line segment adding extra x coordinates
--   on the correct y corrdinates.
--
--   Precondition: both lists are sorted
completeLineWith :: [Double] -> Line -> Line
completeLineWith xs []     = []
completeLineWith xs (n:ns) = completeLineWith' xs n ns

-- |Interpolates the second argument adding points in the
--  same x position as the first argument. The list is
--  already split into head and tail.
--
--  Precondition: both lists are sorted!
completeLineWith' :: [Double] -> Point -> Line -> Line
completeLineWith' []     n ns  = n : ns
completeLineWith' (x:xs) n []
  | x == pointX n = completeLineWith' xs n []
  | x <  pointX n = (x , pointY n) : completeLineWith' xs n []
  | x >  pointX n = n : map (, pointY n) (x:xs)
completeLineWith' (x:xs) n0 (n:ns)
  | x <  pointX n0
    = (x , pointY n0) : completeLineWith' xs n0 (n:ns)
  | x == pointX n0 
    = completeLineWith' xs n0 (n:ns)
  | x >  pointX n0 && x < pointX n
    = let x1 = x - pointX n0
          x2 = pointX n - pointX n0
          y1 = pointY n0
          y2 = pointY n
          newY = y1 + (x1 / x2 * (y2 - y1))
       in n0:completeLineWith' xs (x, newY) (n:ns)
  | x > pointX n0 && x >= pointX n
    = n0:completeLineWith' (x:xs) n ns
  
-- |Computes the volume of the frustum created by the given sections
frustumVolume :: [Line] -> Double
frustumVolume []        = error "frustumVolume: no lines"
frustumVolume [l]       = lengthOfSegment l
frustumVolume [l1 , l2] = areaOfSegment l1 + areaOfSegment l2
frustumVolume ls
  = let theta = (2*pi) / fromIntegral (length ls)
     in case preprocess ls of
          (s1:s2:ss) -> go theta s1 (s2:ss)
          _          -> error "frustumVolume: too few sections"
  where
    preprocess :: [Line] -> [(Double , [Double])]
    preprocess ls 
      = let xs  = nub . sort . concat . map (map pointX) $ ls
            ls' = map (completeLineWith xs) ls
         in map (\sect -> (pointX (head sect) , map pointY sect))
          $ transpose ls'

    -- A section is specified by (x :: Double, ys :: [Double]).
    -- the idea is that the each coordinate in ys is actually seen
    -- as y cis \theta, in a slice for z = x.
    go :: Double -> (Double , [Double]) -> [(Double , [Double])] -> Double
    go theta hd [] = 0
    go theta (z0 , ys0) ((z1 , ys1):rs)
     = let ys01 = zip ys0 ys1 ++ [head ys01]
        in volume theta z0 z1 ys01 + go theta (z1 , ys1) rs

    volume :: Double -> Double -> Double -> [(Double , Double)] -> Double
    volume theta z0 z1 []  = 0
    volume theta z0 z1 [_] = 0
    volume theta z0 z1 ((a0 , a1) : (b0 , b1) : rest)
     = volumeFrustumSection theta z0 z1 a0 a1 b0 b1
     + volume theta z0 z1 ((b0 , b1) : rest)

    volumeFrustumSection :: Double -> Double -> Double -> Double
                        -> Double -> Double -> Double -> Double
    volumeFrustumSection theta z0 z1 a0 a1 b0 b1
     = 0.5 * (sin theta ** 2) * (term z1 - term z0)
     where
       term x = x * a0 * b0
              + x^2 * 1/2 * a0        * (b1 - b0) 
              + x^2 * 1/2 * (a1 - a0) * b0
              + x^3 * 1/3 * (a1 - a0) * (b1 - b0)


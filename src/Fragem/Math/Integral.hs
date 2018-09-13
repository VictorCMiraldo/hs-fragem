-- Provides an interface to compute a n-dimensional integrals.
module Fragem.Math.Integral where

type Point = (Double , Double)

-- * Auxiliary Monsters

-- | Pythagoras!
distance :: Point -> Point -> Double
distance (px , py) (cx , cy)
  = sqrt ((px - cx) ^ 2 + (py - cy) ^ 2) 

-- * Gemoetric Constructions

-- |Returns the length of a line segment.
lengthOfSegment :: [Point] -> Double
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
    go :: Point -> [Point] -> Double
    go _  [] = 0                                                  
    go p (q : rest) = distance p q + go q rest

-- |Returns the area under a segment
--
--  PRECONDITION: the x coordinates are monotonically
--                increasing in the list.
areaOfSegment :: [Point] -> Double
areaOfSegment points
  | length points < 2 = 0
  -- TODO, enforce this precond here instead of requiring it.
  | (p:ps) <- points  = go p ps
  where
    go :: Point -> [Point] -> Double
    go _ [] = 0
    go p@(px , py) (q@(qx , qy) : rest)
      = let deltaX = qx - px
            sqA    = deltaX * min py qy
            trA    = deltaX * abs (py - qy) / 2
         in sqA + trA + go q rest

-- changing the input type: we need at least two notes at t1, and two notes at t2
areaOfXORSegment :: [Point] -> [Point] -> Double
areaOfSegment points1 points2
  | length points1 < 2 || length points2 < 2 = 0
  -- TODO, enforce this precond here instead of requiring it.
  -- Iris: The syntax must be wrong here. But we go recursively we when have two lists of notes that line up nicely with each other 
  | (p:ps) <- points1 (p2:ps2) <- points2 = go p ps p2 ps2
  where
    go :: [Point] -> [Point] -> Double
    go _ [] = 0
    -- Again the syntax is quite wrong. But to calculate the xor area given 4 points on a vertical grid is not hard :) (total height differeces * total time differences) * 0.5 = calculating two triangles at the same time
    go p@(px , py) (q@(qx , qy) : rest) p2@(px2 , py2) (q2@(qx2 , qy2) : rest)
      = let deltaX = qx - px
            deltaY1 = qy - py
            deltaY2 = qy2 - py2
         in ((deltaY1 + deltaY2) * deltaX) * 0.5
  
  -- TODO, given a voice, find the corresponding notes in another voice: another function?
  -- two different types for different voice? 
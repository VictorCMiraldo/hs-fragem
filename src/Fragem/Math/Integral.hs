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

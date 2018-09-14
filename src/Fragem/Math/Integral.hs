-- Provides an interface to compute a n-dimensional integrals.
module Fragem.Math.Integral where

import Control.Arrow ((***))
import Debug.Trace

type Point = (Double , Double)

type LineSegment = (Point , Point)

pointX , pointY :: Point -> Double
pointX = fst
pointY = snd

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

-- |Returns the area in between two lines
areaInBetween :: [Point] -> [Point] -> Double
areaInBetween n m
  | length m < 2 || length n < 2 = 0
  | otherwise
  = let (m0 : ms) = m
        (n0 : ns) = n
        -- TODO!! write a test to ensure this does what we
        --        think it does!
        --        spec: map pointX (m':ms') == map pointX (n':ns')
        m':ms' = completeLineWith (map pointX n) m0 ms
        n':ns' = completeLineWith (map pointX m) n0 ns
     in trace (show $ n':ns') $ go m' ms' n' ns'
  where
    go _ [] _ [] = 0
    go (x,my) ((x' , my'):ms) (_,ny) ((_,ny'):ns)
      -- do not cross!
      | (my >= ny && my' >= ny')
        || (ny >= my && ny' >= my')
      = let area = 0.5 * (x' - x) * (abs (my - my') + abs (ny - ny'))
         in trace (show [x,x',my,my',ny,ny'])
          $ area + go (x',my') ms (x',ny') ns
      | otherwise -- they cross!
      = let dy1 = abs (my - ny)
            dy2 = abs (my' - ny')
            dx  = x'-x
            myX = (dy2 / dy1) * dx
         in trace "do cross"
          $ dy1 * myX * 0.5 + dy2 * (dx - myX) * 0.5
          + go (x',my') ms (x',ny') ns


completeOneLine :: Double -> [Point] -> [Point]
completeOneLine y []          = []
completeOneLine y ((px,_):ps) = (px,y):completeOneLine y ps

-- |Interpolates the second argument adding points in the
--  same x position as the first argument
completeLineWith :: [Double] -> Point -> [Point] -> [Point]
completeLineWith []     n ns  = ns
completeLineWith (x:xs) n []
  = [n]
completeLineWith (x:xs) n0 (n:ns)
  | x == pointX n0 
    = n0:completeLineWith xs n ns
  | x >  pointX n0 && x < pointX n
    = let x1 = x - pointX n0
          x2 = pointX n - pointX n0
          y1 = pointY n0
          y2 = pointY n
          newY = y1 + (x1 / x2 * (y2 - y1))
       in n0:completeLineWith xs (x, newY) (n:ns)
  | x > pointX n0 && x > pointX n
    = n0:completeLineWith (x:xs) n ns
  | otherwise = error $ show x ++ "; " ++ show ns
  

{-
    complete pm ms pn ns
      | pointX pm < pointX pn =  
-}

{-
     in go (m0 , m1) ms (n0 , n1) ns
  where
    go :: LineSegment -> [Point]
       -> LineSegment -> [Point]
       -> Double
    go m@(m0 , m1) ms n@(n0 , n1) ns
      = withCompleteSegments m n
      
      
      = case compare (pointX m0) (pointX n0) of
          EQ -> _
-}

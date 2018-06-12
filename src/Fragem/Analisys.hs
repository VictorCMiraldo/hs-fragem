module Fragem.Analisys where

import Fragem.Syntax
import Fragem.Syntax.Examples
import Fragem.Math

-------------------------------------
-- * Zooming in and out Measures * --
-------------------------------------

-- XXX: BUG: VCM:
--  If the denominator of a time-signature
--  is not four, these might be fundamentally wrong.

-- |How many ticks between each strong beat?
beatStrong :: Section -> Ticks
beatStrong (Section (TimeSig n 4) tpb _)
  = n * tpb

-- |How many ticks between each weak beat
beatWeak :: Section -> Ticks
beatWeak (Section (TimeSig n 4) tpb _)
  = tpb

-- |Accounting for syncopated beats
beatSyncope :: Section -> Ticks
beatSyncope (Section (TimeSig n 4) tpb _)
  = tpb / 2

-- |Keeps only the notes as seen at @detla@ ticks
--  of distance.
measureAt :: Ticks -> Measure -> [Note]
measureAt delta (Measure ns) = go 0 ns
  where
    go acu []               = []
    go 0   (n:ns) = n : go (noteDuration n) ns
    go acu (n:ns)
      | acu + (noteDuration n) <= delta
      = go (acu + noteDuration n) ns
      | otherwise
      = n : go (acu + noteDuration n - delta) ns 

------------------
-- * Fractals * --
------------------

{-

OLD COLD CODE:

type Pitches = [Int]

mass :: Pitches -> Double
mass ps 
  | length ps < 2 = error "Not enough numbers!"
  | otherwise     = go $ map fromIntegral ps
  where
    -- How many triangles?
    count ps = length ps - 1

    ftri :: Double
    ftri = 1 / fromIntegral (count ps)

    -- pythagoras!
    size :: Double -> Double -> Double
    size ca1 ca2 = sqrt (ca1^2 + ca2^2) 
   
    go :: [Double] -> Double
    go [x1,x2] = size ftri (abs (x2 - x1))
    go (x1:x2:xs)
      = size ftri (abs (x2 - x1))
      + go (x2:xs)


type Scaling = Double
type Mass    = Double

-}

{-

 POTTENTIALLY USEFUL STUFF

dim :: Scaling -> Mass -> Mass -> Double
dim s a0 a1 = logBase s (a1 / a0)

dimPitches :: Pitches -> Pitches -> Double
dimPitches p1 p2 = dim 2 (mass p1) (mass p2)

-- VCM: should we just consider triangles
--      or rectangles and triangles too?
fractalMass :: [Note] -> Double
fractalMass ns
  | length ps < 2 = error "Too little notes"
  | otherwise     = go ns
  where
     
-}

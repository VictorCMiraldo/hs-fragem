module Fragem.Analisys where

import Fragem.Syntax
import Fragem.Syntax.Examples
import Fragem.Metrics

import Fragem.Math


------------------
-- * Fractals * --
------------------

-- |Returns nothing if we have less than 2 notes.
notesMass :: [Note] -> Maybe Double
notesMass ps
  | length ps < 2 = Nothing
  | otherwise
  = let (n:ns) = ps
     in Just (go (fromIntegral $ noteDuration n) n ns)
  where
    go :: Double -> Note -> [Note] -> Double
    go acu _    []     = acu
    go acu prev (n:ns)
      = let acu' = acu + hyp prev n
                       + fromIntegral (noteDuration n)
         in go acu' n ns

    hyp :: Note -> Note -> Double
    hyp (Note d1 dur1 p1) (Note d2 dur2 p2)
      = let pitchD = fromIntegral $ p2 - p1
            timeD  = fromIntegral $ (d1 + dur1) - d2        
         in sqrt $ (pitchD ^ 2) + (timeD ^ 2)

type Scaling = Double
type Mass    = Double

dim :: Scaling -> Mass -> Mass -> Double
dim s a0 a1 = logBase s (a1 / a0)

dimPitches :: [Note] -> [Note] -> Double
dimPitches p1 p2
  = case (,) <$> notesMass p1 <*> notesMass p2 of
      Just (p1' , p2') -> dim 2 p1' p2'
      Nothing          -> error "Note groupd must have at least 2 notes!"

{-

-- VCM: should we just consider triangles
--      or rectangles and triangles too?
fractalMass :: [Note] -> Double
fractalMass ns
  | length ps < 2 = error "Too little notes"
  | otherwise     = go ns
  where

-}

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

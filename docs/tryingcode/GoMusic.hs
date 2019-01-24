module GoMusic where

type Pitches = [Int]

data PitchInterval
  = Step Int PitchInterval Int
  | Zero
  | One Int
  deriving (Eq , Show)

toPitches :: Int -> PitchInterval -> [Int]
toPitches depth = go depth [] []
  where
    go 0 bf af _            = reverse bf ++ af
    go n bf af (One i)      = reverse bf ++ i:af
    go n bf af (Step b p a) = go (n-1) (b:bf) (a:af) p
    go n bf af _ = error "To far"

heller :: PitchInterval
heller = Step 0 (Step 0 (One 8) 7) 6

hannon :: PitchInterval
hannon = Step 0 (Step 2 (One 6) 4) 1

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

dim :: Scaling -> Mass -> Mass -> Double
dim s a0 a1 = logBase s (a1 / a0)

dimPitches :: Pitches -> Pitches -> Double
dimPitches p1 p2 = dim 2 (mass p1) (mass p2)

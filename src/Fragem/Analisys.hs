module Fragem.Analisys where

import Fragem.Syntax
import Fragem.Syntax.Examples
import Fragem.Metrics

import Fragem.Math.Integral

------------------
-- * Fractals * --
------------------

toPoints :: [Note] -> [Point]
toPoints = concatMap go
  where
    go :: Note -> [Point]
    go (Note delay dur pitch)
      = let d'   = fromIntegral delay
            dur' = fromIntegral dur
            p'   = fromIntegral pitch
         in [ (d' , p') , (d' + dur' , p') ]
            
notesMass :: FrustumMassType -> [[Note]] -> Maybe Double
notesMass fmt = Just . volume fmt . map toPoints

type Scaling = Double
type Mass    = Double

dim :: Scaling -> Mass -> Mass -> Double
dim s a0 a1 = logBase s (a1 / a0)

dimPitches :: FrustumMassType -> [[Note]] -> [[Note]] -> Double
dimPitches fmt p1 p2
  = case (,) <$> notesMass fmt p1 <*> notesMass fmt p2 of
      Just (p1' , p2') -> dim 2 p1' p2'
      Nothing          -> error "Note groupd must have at least 2 notes!"

test :: [Note]
test = [ Note 0   40 40
       , Note 40  40 90
       , Note 100 20 95
       ]

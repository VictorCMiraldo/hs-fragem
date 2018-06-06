module Fragem.Syntax where

-- |A 'Duration' indicates the relative duration of a note.
--  Each dot increases the duration 
data Duration
  -- |A @Simple i@ lasts for @1/(2 ** i)@ of a measure.
  = Simple Int 
  -- |A @Dot d@ lasts for @d + .5*d@ of a measure.
  | Dot Duration
  -- |A @Irregular n d@ lasts for
  | Irregular Int Duration
  deriving (Eq , Show , Ord)

-- |Returns the fraction of the measure used by the duration.
durationValue :: Duration -> Float
durationValue (Simple i) = 1 / (2 ** (fromIntegral i))
durationValue (Irregular n d)
  = durationValue d * (fromIntegral n - 1) / fromIntegral n
durationValue (Dot d)
  = let nd  = countDots d + 1
        nd' = 2 ** nd
     in durationValue (noDots d) * (1 + ((nd' - 1) / nd'))
  where countDots (Dot d) = 1 + countDots d
        countDots _       = 0
        noDots (Dot d)    = noDots d
        noDots x          = x
                        

-- |Interpolates a float in a duration. We play a game close
--  to Newton's method here.
--
--  TODO: Add how many dots should we consider; this changes
--        everything.
durationInterp :: Float -> Duration
durationInterp f
  = let start = Simple (-2)
     in go f start (dist (durationValue start) f)
  where
    dist :: Float -> Float -> Float
    dist x y = abs (x - y)

    down :: Duration -> Duration
    down (Simple i) = Dot (Simple (i + 1))
    down (Dot x)    = x

    go :: Float -> Duration -> Float -> Duration
    go tgt cand d
      = let cand' = down cand
            d' = dist (durationValue cand') tgt
         in if d' < d 
            then go tgt cand' d'
            else cand

-- |A 'NotePitch' can be a rest or a pitch.
data NotePitch
  = Rest
  | Pitch Int
  deriving (Eq , Show , Ord)

-- |A 'Note' has a duration, a pitch and it might be tied
--  to the note that comes after.
data Note = Note
  -- | Duration of a note
  { noteDuration :: Duration
  -- | Pitch of the note (or rest)
  , notePitch    :: NotePitch
  -- | Whether this note is tied to the next one or not.
  , noteTie      :: Bool
  } deriving (Eq , Show , Ord)

-- |A 'Measure' consists in a list of notes.
newtype Measure = Measure { measureNotes :: [Note] }
  deriving (Eq , Show , Ord)

-- |A 'Section' consists in a a list of measures in a given time signature.
data Section = Section
  { sectionSignature :: TimeSig
  , sectionMeasires  :: [Measure]
  } deriving (Eq , Show , Ord)

-- |Musical time signature.
data TimeSig = TimeSig Int Int
  deriving (Eq , Show , Ord)

-------------------------
-- * Some Processing * --
-------------------------

-- VCM: TODO: Refactor this somewhere else

(.+) :: Duration -> Duration -> Duration
p .+ q = durationInterp (durationValue p + durationValue q)

-- |@interpolateBy n ms@ will return a list of pitches as
--  seen at every @n@th beat.
interpolateBy :: Duration -> [Measure] -> [[Int]]
interpolateBy = undefined

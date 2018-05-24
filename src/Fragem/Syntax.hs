module Fragem.Syntax where

-- |A 'Duration' indicates the relative duration of a note.
--  Each dot increases the duration 
data Duration
  -- |A @Simple i@ lasts for @1/(2 ** i)@ of a measure.
  = Simple Int 
  -- |A @Dot d@ lasts for @d + .5*d@ of a measure.
  | Dot Duration
  deriving (Eq , Show , Ord)

{-
  -- |Lasts 8 measures
  = Maxima
  -- |Lasts 4 measures
  | Longa
  -- |Lasts 2 measures
  | Double
  -- |Lasts 1 measure
  | Whole
  -- |Lasts 1/2 measure
  | Half
  -- |Lasts 1/4 measure
  | Quarter
  -- |Lasts 1/8 measure
  | Eighth
  -- |Lasts 1/16 measure
  | Sixteenth
  -- |Lasts 1/32 measure
  | TirtySecond
  -- |Lasts 1/64 measure
  | SixtyFourth
-}

-- |Returns the reciprocal of the relative value of a duration,
--  that is, @durationValue Quarter = 4@.
durationRecip :: Duration -> Float
durationRecip (Dot d)    = 2/3 * durationRecip d
durationRecip (Simple i) = 2 ** (fromIntegral i)
{-
durationRecip Maxima      = 0.125
durationRecip Longa       = 0.25
durationRecip Double      = 0.5
durationRecip Whole       = 1
durationRecip Half        = 2
durationRecip Quarter     = 4
durationRecip Eighth      = 8
durationRecip Sixteenth   = 16
durationRecip TirtySecond = 32
durationRecip SixtyFourth = 64
-}

-- |Returns the fraction of the measure used by the duration.
durationValue :: Duration -> Float
durationValue = (1/) . durationRecip

-- |Interpolates a float in a duration. We play a game close
--  to Newton's method here.
durationInterp :: Float -> Duration
durationInterp f
  = let start = Simple (-2)
     in go f start (dist (durationValue start) f)
  where

dist :: Float -> Float -> Float
dist x y = abs (x - y)

down :: Duration -> Duration
down (Dot (Simple i))             = (Simple i) 
down (Dot (Dot (Simple i)))       = (Dot (Simple i)) 
down (Dot (Dot (Dot (Simple i)))) = (Dot (Dot (Simple i))) 
down (Simple i)                   = Dot (Dot (Dot (Simple (i+1)))) 

go :: Float -> Duration -> Float -> Duration
go f cand d
  = let d' = dist (durationValue (down cand)) f
     in if d' < d 
        then go f (down cand) d'
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

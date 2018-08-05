module Fragem.Syntax where

-- |An 'Offset' is an absolute amount of ticks counting
--  from the start of the piece.
type Offset = Int

-- |Ticks is the unit of time used throughout MIDI
type Ticks  = Int

-- |A 'Note' has a duration, a pitch and it might be tied
--  to the note that comes after.
data Note = Note
  -- | Absolute offset of note from start of the piece; in ticks
  { noteDelay    :: Offset
  -- | Duration of a note
  , noteDuration :: Ticks
  -- | Pitch of the note (or rest)
  , notePitch    :: Int
  } deriving (Eq , Ord)

instance Show Note where
  show (Note nd ndur p) = show nd ++ "+" ++ show ndur ++ ";" ++ show p

-- |Musical time signature.
data TimeSig = TimeSig
  { timesigNum   :: Int
  , timesigDenum :: Int
  } deriving (Eq , Show , Ord)

-- |A 'Measure' consists in a list of notes.
newtype Measure = Measure { measureNotes :: [Note] }
  deriving (Eq , Show , Ord)

-- |Concatenate the notes of n measures into a
--  single group of notes; useful for computing the mass
--  over intervals that are bigger than a measure.
regroupMeasure :: Int -> [Measure] -> [[Note]]
regroupMeasure n ls
  | length ls < n = []
  | otherwise
  = let ls0 = take n ls
        ls1 = drop n ls
     in concat (map measureNotes ls0) : regroupMeasure n ls1

-- |Same as 'regroupMeasure', but employs a sliding
--  window technique.
regroupMeasureSlide :: Int -> [Measure] -> [[Note]]
regroupMeasureSlide n ls
  | length ls < n = []
  | otherwise
  = let ls0 = take n ls
        ls1 = tail ls
        in concat (map measureNotes ls0) : regroupMeasureSlide n ls1

-- |Extract a given number of measures from a section
extractMeasures :: Int -> Section -> [Measure]
extractMeasures i = take i . sectionMeasures


-- |A 'Section' consists in a a list of measures in a given time signature.
data Section = Section
  -- |Current section's time signature
  { sectionSignature :: TimeSig
  -- |Current section's ticks-per-beat
  , sectionTPB       :: Int
  -- |Measures in the given tempo
  , sectionMeasures  :: [Measure]
  } deriving (Eq , Show , Ord)

-- |A 'Voice' is a list of sections.
newtype Voice = Voice { pieceSections :: [Section] }
  deriving (Eq , Show , Ord)

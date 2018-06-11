module Fragem.Syntax where

type Offset = Int
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
  } deriving (Eq , Show , Ord)

-- |Musical time signature.
data TimeSig = TimeSig
  { timesigNum   :: Int
  , timesigDenum :: Int
  } deriving (Eq , Show , Ord)

-- |A 'Measure' consists in a list of notes.
newtype Measure = Measure { measureNotes :: [Note] }
  deriving (Eq , Show , Ord)

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

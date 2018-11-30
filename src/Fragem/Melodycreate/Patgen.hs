
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# Language FlexibleInstances #-}

import Codec.Midi
import Data.Aeson
import Data.Bits
import Test.QuickCheck
import GHC.Generics


type MidiEvent = (Ticks, Message)

type Name = String

data Melody
    = Play Note
    | Rest Duration
    | Pattern [Melody]
    | Repeat Int Melody
    deriving (Show, Eq, Generic)


data Duration
    = Whole   -- breve
    | Half    -- minim
    | Quarter -- crotchet
    | Eighth   -- quaver
    | Sixteenth -- semiquaver
    | Thirsec   -- demisemiquaver
    deriving (Show, Eq, Generic)

data Meter
    = Simple Int Int  -- numerator denominator
    deriving (Show, Eq, Generic)

data Note = Note Pitch Duration Accent
  deriving (Show, Eq, Generic)

newtype Pitch = Pitch { getPitch :: Int }
  deriving (Show, Eq, Generic)

newtype Accent = Accent { getAccent :: Int }
  deriving (Show, Eq, Generic)


trackOfMelody :: Ticks -> Melody -> Track Ticks
trackOfMelody beat = go
  where
    go = \case
      Play note         -> trackOfNote beat note
      Pattern melody      -> concatMap go melody
      Repeat count melody -> concat (replicate count (go melody))

trackOfNote :: Ticks -> Note -> Track Ticks
trackOfNote beat (Note pitch duration accent) =
  [ keydown pitch accent
  , keyup   pitch beat duration
  ]

ticksOfDuration :: Ticks -> Duration -> Ticks
ticksOfDuration beat = \case
  Whole   -> beat * 4
  Half    -> beat * 2
  Quarter -> beat
  Eighth   -> beat `quot` 2
  Sixteenth -> beat `quot` 4
  Thirsec  -> beat `quot` 8

midiOfMelody :: Ticks -> Meter -> Melody -> Midi
midiOfMelody beat meter
  = midiSkeleton beat meter
  . trackOfMelody beat

midiSkeleton :: Ticks -> Meter -> Track Ticks -> Midi
midiSkeleton beat meter track =
  Midi { fileType = MultiTrack
       , timeDiv  = TicksPerBeat beat
       , tracks   = [
          [
           (0,ChannelPrefix 0),
           (0,TrackName " Grand Piano  "),
           (0,InstrumentName "GM Device  1"),
           (0,midiTimeSignature meter),
           (0,KeySignature 0 0)
          ]
          ++
          track
          ++
          [
           (0,TrackEnd)
          ]
         ]
       }

-- http://www.deluge.co/?q=midi-tempo-bpm
midiTimeSignature :: Meter -> Message
midiTimeSignature (Simple n m) =
  TimeSignature n (countTrailingZeros m) 24 8

keydown :: Pitch -> Accent -> MidiEvent
keydown pitch accent
  = ( 0
    , NoteOn { channel  = 0
             , key      = getPitch pitch
             , velocity = getAccent accent
             })

keyup :: Pitch -> Ticks -> Duration -> MidiEvent
keyup pitch beat duration
  = ( ticksOfDuration beat duration
    , NoteOff { channel  = 0
              , key      = getPitch pitch
              , velocity = 0
              })

instance ToJSON Duration
instance FromJSON Duration

instance ToJSON Note
instance FromJSON Note

instance ToJSON Accent
instance FromJSON Accent

instance ToJSON Pitch
instance FromJSON Pitch

instance ToJSON Melody
instance FromJSON Melody

writeMelody :: FilePath -> Melody -> IO ()
writeMelody = encodeFile

readMelody :: FilePath -> IO Melody
readMelody path = do
  result <- eitherDecodeFileStrict path
  case result of
    Left message -> error message
    Right melody -> return melody


-- Transposition
-- -------------
type Interval = Int

transposeNote :: Interval -> Note -> Note
transposeNote i (Note (Pitch p) t a)
  = Note (Pitch (i+p)) t a

chromatictransposeMelody :: Interval -> Melody -> Melody
chromatictransposeMelody by = \case
  Play note     -> Play (transposeNote by note)
  Pattern melody  -> Pattern (map (chromatictransposeMelody by) melody)
  Repeat n melody -> Repeat n (chromatictransposeMelody by melody)

-- TODO: tonal transposition, moving on an uneven grid

-- Tests
-- -----

testMelody :: Melody
testMelody =
  Pattern [ Play (quarter c5)
          , Play (half c5)
          , Repeat 3 (Play (quarter c5))
          ]

commonTime :: Meter
commonTime = Simple 4 4

half :: Note -> Note
half (Note p _ a) = Note p Half a

quarter :: Note -> Note
quarter (Note p _ a) = Note p Quarter a

unaccented :: Accent
unaccented = Accent 1

c5 :: Note
c5 = Note (Pitch 60) Whole unaccented

-- mode modelling
type Intervals = [Interval]
type Mode = Intervals

type Pitches = [Pitch]
type Rhythm = [Duration]
type Dynamics = [Accent]

-- pattern modelling
-- should go into data structure
type Prototype = [Note]
type Variation = Prototype -> Prototype

-- also know as the major scale
ionian :: Mode
ionian = concat (repeat [2,2,1,2,2,2,1])

-- also know as the natural minor scale
aeolian :: Mode
aeolian = concat (repeat [2,1,2,2,1,2,2])

cumInterval :: Intervals -> Pitches
cumInterval i = map Pitch sums
  where sums = scanl (+) 0 i

createPrototype :: Pitches -> Rhythm -> Dynamics -> [Note]
createPrototype = zipWith3 Note

type Length = Int
type StartPitch = Pitch

-- probably not the right way to do this
makeBasicScale :: Mode -> StartPitch -> Duration -> Length -> Melody
makeBasicScale m (Pitch p) d l = Pattern (map Play (take l (map (transposeNote p) notes)))
  where
    notes = createPrototype (cumInterval m) (repeat d) (repeat unaccented)

makefromIntervals :: Intervals -> StartPitch -> Duration -> Length -> Melody
makefromIntervals m (Pitch p) d l = Pattern (map Play (take l (map (transposeNote p) notes)))
  where
    notes = createPrototype (cumInterval m) (repeat d) (repeat unaccented)

ionianup = makeBasicScale ionian (Pitch 60) Whole 11
jumps = makefromIntervals jump8 (Pitch 60) Whole 11

-- mode interval data
dorian :: Mode
dorian =  [2,1,2,2,2,1,2]

lydian :: Mode
lydian =  [2,2,1,2,2,2,1]

mixoly :: Mode
mixoly =  [2,2,1,2,2,1,2]

jump8 :: Intervals
jump8 =  [8, -8]

-- TODO: combination
-- parameters: what patterns, from where to where, already in the datatype
ij8 :: Melody
ij8 =
  Pattern [ Repeat 2 ionianup
          , Repeat 2 jumps
          , Play (Note (Pitch 60) Half (Accent 0))
          ]

-- random
genPitch :: Gen Pitch
genPitch = do
  p <- (choose (0, 127))
  return $ Pitch p

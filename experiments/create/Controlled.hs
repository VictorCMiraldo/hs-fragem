
{-# Language FlexibleInstances #-}
module Fragem.Melodycreate.Controlled where

import Codec.Midi
import Data.List
type Pitch = Int
type Note = (Pitch, Ticks, Acc)
type Degree = Int
type Acc = Int

type StartPitch = Pitch
type Length = Int

type Interval = Int
type Pat = [Interval]

--type Melody = [Note]
--type Pattern = [Note]

data Melody = Sequence [Melody]
            | Repeat Int Melody
            | Pattern [Note]
-- is there a way to do Repeat Int Pattern?

type MidiEvent = (Ticks, Message)
midiSkeleton :: Track Ticks -> Midi
midiSkeleton mel =  Midi {
         fileType = MultiTrack,
         timeDiv = TicksPerBeat 480,
         tracks = [
          [
           (0,ChannelPrefix 0),
           (0,TrackName " Grand Piano  "),
           (0,InstrumentName "GM Device  1"),
           (0,TimeSignature 4 2 24 8),
           (0,KeySignature 0 0)
          ]
          ++
          mel
          ++
          [
           (0,TrackEnd)
          ]
         ]
       }

keydown :: Pitch -> Acc -> MidiEvent
keydown k d =  (0,NoteOn {channel = 0, key = k, velocity = d})

keyup :: Pitch -> Ticks -> MidiEvent
keyup k t =  (t, NoteOff {channel = 0, key = k, velocity = 0})

playnote :: Note -> Track Ticks
playnote (x,y,z) = [ keydown x z, keyup x y]

createMidi :: FilePath -> Melody -> IO()
--createMidi f notes = exportFile  f $ midiSkeleton $ concat $ map  playnote notes
createMidi f (Pattern m) = exportFile  f $ midiSkeleton $ concat $ map  playnote m
--createMidi f (Repeat r m) = exportFile  f $ midiSkeleton $ concat $ map  playnote (replicate r m)
--createMidi f (Sequence m) = exportFile  f $ midiSkeleton $ concat $ map  playnote (concat m)
-- how to go down to the note level?
--is there a better way to do this?

toImplement3 :: (b0 -> c0, b1 -> c1, b2 -> c2) -> (b0, b1, b2) -> (c0, c1, c2)
toImplement3 (f1, f2, f3) (a1, a2, a3) = (f1 a1, f2 a2, f3 a3)

transposeMelody :: Degree -> Melody -> Melody
transposeMelody d (Pattern m) = Pattern (fmap (toImplement3 ((+d), (+0), (+0))) m)
transposeMelody d (Repeat r m) = Sequence (replicate r (transposeMelody d m)) --lose the repetition?
transposeMelody d (Sequence m) = Sequence (fmap (transposeMelody d) m)
-- same function, different types, hmm...
transposeNotes :: Degree -> [Note] -> [Note]
transposeNotes d n = fmap (toImplement3 ((+d), (+0), (+0))) n

-- not yet "melody"
createPatfromOne :: Pat -> Length -> [Note]
createPatfromOne p l = zip3 (take (l) [ i | i <- scanl1 (+) (cycle p)]) (replicate l 480) (replicate l 2)

-- was hard to convert between pattern and melody...
createByPat :: Pat -> StartPitch -> Length -> Melody
createByPat p f l = Pattern ((f, 480, 2) : transposeNotes f (createPatfromOne p l))

ionian :: Melody
ionian = createByPat [2,2,1,2,2,2,1] 60 19

aeolian :: Melody
aeolian = createByPat [2,1,2,2,1,2,2] 60 19

dorian :: Melody
dorian = createByPat [2,1,2,2,2,1,2] 60 19

lydian :: Melody
lydian = createByPat [2,2,1,2,2,2,1] 60 19

mixoly :: Melody
mixoly = createByPat [2,2,1,2,2,1,2] 60 19

jump8 :: Melody
jump8 = createByPat [8, -8] 60 19

testMelody :: Melody
testMelody = Pattern [(60, 480, 2), (60, 960, 3), (60, 480, 1), (60, 480, 2), (60, 480, 2)]

testMelody2 :: Melody
testMelody2 = Pattern [(60, 480, 2), (60, 960, 3), (60, 480, 1), (60, 480, 2), (60, 480, 2)]


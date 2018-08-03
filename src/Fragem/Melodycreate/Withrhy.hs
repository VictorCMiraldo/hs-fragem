module Fragem.Melodycreate.Withrhy where 

import Codec.Midi
import Test.QuickCheck

type Pitch = Int
type Note = (Pitch, Ticks, Acc)
type Degree = Int
type Acc = Int
type Melody = [Note]
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
createMidi f notes = exportFile  f $ midiSkeleton $ concat $ map  playnote notes

testMelody :: Melody 
testMelody = [(60, 480, 2), (60, 960, 3), (60, 480, 1), (60, 480, 2), (60, 480, 2)]

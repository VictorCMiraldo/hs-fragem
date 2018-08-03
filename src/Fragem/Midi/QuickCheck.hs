module Fragem.Midi.QuickCheck (genMidiTrack) where

import Codec.Midi

import Test.QuickCheck

type Pitch = Int
type Note = (Pitch, Ticks, Acc)
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

createMidi :: Melody -> Midi
createMidi notes = midiSkeleton $ concat $ map  playnote notes

genPitch :: Gen Pitch
genPitch = frequency [(1 , choose (0,24))
                     ,(2 , choose (24,60))
                     ,(1 , choose (60,84))
                     ]

genTicks :: Gen Ticks
genTicks = oneof $ map return [120, 240, 360, 480, 600, 720, 840, 960, 1080]

genVelocity :: Gen Int
genVelocity = choose (0 , 5)

genNote :: Gen Note
genNote = (,,) <$> genPitch <*> genTicks <*> genVelocity

genMidiTrack :: IO Midi
genMidiTrack = generate genMidi

genMidi :: Gen Midi
genMidi = createMidi listOf genNote

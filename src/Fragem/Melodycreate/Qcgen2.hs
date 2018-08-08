{-# Language FlexibleInstances #-}
module Fragem.Melodycreate.Qcgen2 where

import Control.Arrow ((***))
import Test.QuickCheck
import Codec.Midi

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

genBar :: Gen [Note]
genBar = go 0
  where
    barDuration = 4 * 480
    
    go dur
      | dur >= barDuration = return []
      | otherwise
      = do t <- genTicks `suchThat` (\ t -> t + dur <= barDuration)
           n <- genPitch
           v <- genVelocity
           rest <- go (dur + t)
           return $ (n , t , v) : rest


-- |Receives a bunch of non-random bars and with
--  a probability puts them in a random place within
--  random bars
--
--  > genComplicatedMelody [(2 , ionianUp) , (1, ionianDown)] 1
--
--  Generates a piece where 50% of the bars are ionianUp, 25% are
--  ionianDown and 25% are random.
genComplicatedMelody :: [(Int , [Note])] -> Int -> Gen Melody
genComplicatedMelody myBars randomFreq
  = concat <$> (listOf $ frequency $ (randomFreq , genBar) : map (id *** return) myBars)

genMelody :: Gen Melody
genMelody = listOf genNote

-- Generate n random pieces
createrandom :: FilePath -> IO()
createrandom f = generate genMelody >>= createMidi f

createNrandom :: Int -> IO()
createNrandom n = mapM_ createrandom ((map ("./"++) filelist))
  where filelist = zipWith (++) (map show [1..n]) (replicate n ".mid")

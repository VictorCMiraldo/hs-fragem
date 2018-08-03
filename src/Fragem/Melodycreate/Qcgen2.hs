
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
import System.Random

import Test.QuickCheck
import Codec.Midi
import Data.Array
import Data.Default

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

-- Quickcheck  Part
instance Arbitrary Pitch where
    arbitrary = elements (Pitch <$> choose (0,80))

-- genMelody ::  Gen Melody
-- genMelody = do
--     chord  <- arbitrary
--     chordqual <- arbitrary
--     -- such that
    -- return $ Chordseq chord chordqual


-- makeNote a b c = Note (a,b,c)


-- instance Arbitrary Melody where
--     arbitrary = do
--             pitch <- choose (0, 80)
--             -- dur <- choose [120, 240, 360, 480, 600, 720, 840, 960, 1080]
--             dur <- choose (120, 480)
--             acc <- choose (1, 10)
--             return $ [makeNote pitch dur acc]

-- example1: T
-- generate arbitrary :: IO ([Bool],Double)

-- example2: F
-- newtype Prime a = Prime a deriving Show
-- instance (Integral a, Arbitrary a) => Arbitrary (Prime a) where
--   arbitrary = do
--     x <- frequency [ (10, choose (0, 1000))
--                    , (5, choose (1001, 10000))
--                    , (1, choose (10001, 50000))
--                    ]
--     return $ x

-- prop_PrimeSum_v4 :: Prime Int -> Prime Int -> Property
-- prop_PrimeSum_v4 (Prime p) (Prime q) =
--     p > 2 && q > 2 ==> classify (p < 1000 || q < 1000) "has small prime" $ even (p + q)

-- example1 conti:
-- a number between 1 and 100
-- newtype Guess = Guess Int
--   deriving (Eq, Num, Ord, Read, Show)

-- instance Random Guess where
--   randomR (Guess lo, Guess hi) g = let (x, g') = randomR (lo, hi) g
--                                    in (Guess x, g')
--   random = randomR (Guess 1, Guess 100)

-- instance Arbitrary Guess where
--   arbitrary = Guess <$> choose (1, 100)
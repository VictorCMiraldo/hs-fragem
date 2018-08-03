module Fragem.Midi.QuickCheck (genMidiTrack) where

import qualified Codec.Midi      as Midi

import Test.QuickCheck

genMidiTrack :: IO Midi.Midi
genMidiTrack = generate genMidi

genMidi :: Gen Midi.Midi
genMidi = _

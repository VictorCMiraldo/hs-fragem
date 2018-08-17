module Fragem.Syntax.ToMidi (sectionToMidi) where

import Control.Arrow ((***), (&&&))
import Control.Monad
import Control.Monad.State
import Data.Function (on)
import Data.List (sortBy, groupBy, span)

import qualified Codec.Midi      as Midi
import qualified Data.Map.Strict as M

import Fragem.Syntax

-- |Translates a bunch of notes to a midi file.
translate :: [Note] -> Midi.Track Midi.Ticks
translate = sortBy (compare `on` fst) . go
  where
    go [Note delay dur pitch]
      = (delay       , Midi.NoteOn  2 pitch 1)
      : (delay + dur , Midi.NoteOff 2 pitch 1)
      : (delay + dur , Midi.TrackEnd)
      : []
    go (Note delay dur pitch : ms)
      = (delay       , Midi.NoteOn  2 pitch 1)
      : (delay + dur , Midi.NoteOff 2 pitch 1)
      : go ms

-- |Issues a midi time signature message; the magic numbers
--  are the standard values (taken from: http://www.deluge.co/?q=midi-tempo-bpm)
mkTimeSigMsg :: TimeSig -> [(Midi.Ticks , Midi.Message)]
mkTimeSigMsg (TimeSig num denum)
  = [(0 , Midi.TimeSignature num (log2 denum) 24 8)
    ,(0 , Midi.TrackEnd)
    ]
  where
    log2 :: Int -> Int
    log2 n = round $ logBase 2 (fromIntegral n)

-- |Translates a section to a midi
--  to a file.
sectionToMidi :: FilePath -> Section -> IO ()
sectionToMidi file (Section ts tpb ms)
  = Midi.exportFile file
  $ Midi.Midi Midi.MultiTrack
              (Midi.TicksPerBeat tpb)
              [ mkTimeSigMsg ts
              , Midi.fromAbsTime $ translate (concat $ map measureNotes ms)
              ]

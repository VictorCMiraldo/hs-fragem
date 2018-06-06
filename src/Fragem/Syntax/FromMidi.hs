module Fragem.Syntax.FromMidi (fromMidi) where

import Control.Monad
import Data.Function (on)
import Data.List (sortBy, groupBy)

import qualified Codec.Midi as Midi
import qualified Data.Map   as M

import           Fragem.Syntax

-- |A midi time signature consists in four values.
--  @TimeSignature nn dd cc bb@ where
--
--    nn - numerator
--    dd - denominator (the real denominator is 2^dd)
--    cc - MIDI tickes per metronome click (default is 24)
--    bb - 32nd node per MIDI quarter note
--
-- http://www.deluge.co/?q=midi-tempo-bpm

-- |Is a 'Midi.Message' a time signature?
isTimeSig :: Midi.Message -> Bool
isTimeSig (Midi.TimeSignature _ _ _ _) = True
isTimeSig _                            = False

-- |Is a 'Midi.Message' a note event?
isNoteEvent :: Midi.Message -> Bool
isNoteEvent (Midi.NoteOn  _ _ _) = True
isNoteEvent (Midi.NoteOff _ _ _) = True
isNoteEvent _                    = False

-- |Returns the first time signature it finds in a track.
--
-- TODO: how to support different time signatures?
getTimeSig :: [Midi.Track a] -> TimeSig
getTimeSig = maybe (error "getTimeSig: no time signature!") id
           . head
           . filter (maybe False (const True))
           . map getTimeSig1
  where
    getTimeSig1 :: Midi.Track a -> Maybe TimeSig
    getTimeSig1 tr = case filter (isTimeSig . snd) tr of
                       ((_ , Midi.TimeSignature n d _ _) : _)
                          -> Just $ TimeSig n (2^d)
                       _ -> Nothing

type Ticks = Midi.Ticks
type Key   = Midi.Key

-- |A 'NoteEvent' consists in some time information,
--  some duration in ticks, and a pitch.
type NoteEvent = (Ticks , Ticks , Key)

-- |Gets the note events.
--  the note is on or off. True indicates on.
getNoteEvents :: Midi.Track Midi.Ticks -> [NoteEvent]
getNoteEvents = sortBy go . process M.empty 0 . simplify
  where
    go = compare `on` (\(x , y , z) -> x)
    
    -- Processing the stream of notes is simple. Everytime we see
    -- a NoteOn event we add it on a map and record when it was "on".
    -- Once we see a "NoteOff" event we look up when was the note on,
    -- and compute the interval.
    --
    -- TODO: We can identify ties by noticing that the duration of
    --       a note spans accross a measure bound.
    process :: M.Map Key Ticks
            -> Ticks
            -> [(Midi.Ticks , Bool , Int)]
            -> [NoteEvent]
    process m t []
      | M.null m  = []
      | otherwise = error "getNoteEvents: unclosed notes"
    process m t ((delay , isOn , key):ks)
      = let t' = t + delay
         in if isOn
            then process (M.insert key t' m) t' ks
            else case M.lookup key m of
                      Just t0 -> (t0 , t' - t0 , key)
                               : process (M.delete key m) t' ks
                      Nothing -> error "getNoteEvents: NoteOff without corresponding NoteOn"
    
    -- returns a list of the noteon and noteoff events in a midi track.
    -- the boolean flag indicates a noteon.
    simplify :: Midi.Track a -> [(a , Bool , Int)]
    simplify = map convert . filter (isNoteEvent . snd)
      where
        convert (x , Midi.NoteOn  _ key _) = (x , True  , key)
        convert (x , Midi.NoteOff _ key _) = (x , False , key)
    

convertTrack :: TimeSig -> Midi.TimeDiv -> Midi.Track Midi.Ticks
             -> Section
convertTrack (TimeSig n d) (Midi.TicksPerSecond _ _) tr
  = error "convertTrack: TicksPerSecond is unsupported"
convertTrack (TimeSig n d) (Midi.TicksPerBeat bt) tr
  = Section (TimeSig n d)
  . map (Measure . map convert)
  . groupBy sameMeasure
  . getNoteEvents
  $ tr
  where
    nbt = n * bt -- ticks per measure
     
    -- does two note events occur in the same measure?
    sameMeasure :: NoteEvent -> NoteEvent -> Bool
    sameMeasure (i0 , _ , _) (i1 , _ , _)
      = (i0 `div` nbt) == (i1 `div` nbt)

    -- TODO: we are not supporting note ties yet either....
    convert :: NoteEvent -> Note
    convert (_ , delta , k) = Note (durationInterp (fromIntegral delta / fromIntegral nbt))
                                   (Pitch k)
                                   False

-- |Main function here; given a midi file, returns a list of
--  sections. One for each track of the file.
--
--  TODO: We still do NOT support changes in time signatures
--        nor sectionswith multiple notes at the same time.
fromMidi :: FilePath -> IO (Either String [Section])
fromMidi file = either (Left . id) (Right . go)
            <$> Midi.importFile file
  where
    go :: Midi.Midi -> [Section]
    go midi = let tracks = Midi.tracks midi 
                  tsig   = getTimeSig tracks
               in map (convertTrack tsig (Midi.timeDiv midi)) tracks

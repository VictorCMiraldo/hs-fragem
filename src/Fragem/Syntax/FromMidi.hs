{-# LANGUAGE RankNTypes #-}
module Fragem.Syntax.FromMidi (fromMidi) where

import Control.Arrow ((***), (&&&))
import Control.Monad
import Control.Monad.State
import Data.Function (on)
import Data.List (sortBy, groupBy)

import qualified Codec.Midi      as Midi
import qualified Data.Map.Strict as M

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

--------------------------------
-- * Processing a Midi File * --
--------------------------------
--
-- $stepone
--
-- The first step is to make everything in terms of absolute offsets,
-- this makes thinks much simpler. Even before that, we better make
-- sure that this is a 'Midi.MultiTrack' file, that is, each track
-- is separate from each other. We leave the processing of other 'Midi.FileType's
-- as future work.
--
-- Making the deltas in the midi file into absolute delays is simple.
-- We do so by traversing every track and adding up all the offsets.
-- The function 'midiAbsoluteOffsets' performs the conversion.
--

-- | Returns whether a Midi file is a multi-track file. (Type 1)
midiIsMultiTrack :: Midi.Midi -> Bool
midiIsMultiTrack = (Midi.MultiTrack ==) . Midi.fileType

-- | Returns whether a Midi file has a 'TicksPerBeat' tempo indication.
--   We do not support ticks-per-second yet. TODO
--
--   Found a description of that here:
--     https://www.music.mcgill.ca/~ich/classes/mumt306/StandardMIDIfileformat.html#BM2_1
midiIsTicksPerBeat :: Midi.Midi -> Bool
midiIsTicksPerBeat = isTPB . Midi.timeDiv
  where
    isTPB (Midi.TicksPerBeat _) = True
    isTPB _                     = False

-- | Makes all the track's delays into absolute offsets.
--   IMPORTANT: this only works for multi-track files. See 'midiIsMultiTrack'.
midiAbsoluteOffsets :: Midi.Midi -> Midi.Midi
midiAbsoluteOffsets (Midi.Midi ft td tracks)
  = Midi.Midi ft td (map (tracksAbsoluteOffsets 0) tracks)
  where
    tracksAbsoluteOffsets :: Midi.Ticks -> Midi.Track Midi.Ticks -> Midi.Track Midi.Ticks
    tracksAbsoluteOffsets acu [] = []
    tracksAbsoluteOffsets acu ((t , msg):rest)
      = (t + acu , msg) : tracksAbsoluteOffsets (t + acu) rest

-- $steptwo
--
-- Now, we have to gather the TimeSig messages and divide the tracks into chunks
-- in the same time signatures. We call these 'Sections'. This requires much
-- more processing. We need to first mine the tracks for TimeSig messages; record
-- their absolute offset on a interval map and split the sections.
--

-- |A 'NoteEvent' consists in some time information,
--  some duration in ticks, and a pitch.
type NoteEvent = (Midi.Ticks , Midi.Ticks , Midi.Key)

offset :: NoteEvent -> Midi.Ticks
offset (o , _ , _) = o

-- |Get all the note events in a track; convert them to an easier to handle format.
--  PRECONDITION: the track must be in absolute-time-reference format.
midiGetNoteEvents :: Midi.Track Midi.Ticks -> [NoteEvent]
midiGetNoteEvents = sortBy go . process M.empty . simplify
  where
    go = compare `on` offset
    
    -- Processing the stream of notes is simple. Everytime we see
    -- a NoteOn event we add it on a map and record when it was "on".
    -- Once we see a "NoteOff" event we look up when was the note on,
    -- and compute the interval.
    --
    -- TODO: We can identify ties by noticing that the duration of
    --       a note spans accross a measure bound.
    process :: M.Map Midi.Key Ticks
            -> [(Midi.Ticks , Bool , Int)]
            -> [NoteEvent]
    process m []
      | M.null m  = []
      -- TODO: print out the actual unclosed notes.
      | otherwise = error "getNoteEvents: unclosed notes"
    process m ((offset , isOn , key):ks)
      | isOn      = process (M.insert key offset m) ks
      | otherwise = case M.lookup key m of
                      Just t0 -> (t0 , offset - t0 , key)
                               : process (M.delete key m) ks
                               -- VCM: Some of the Well Temepred Clavier have NoteOffs without
                               --      corresponding noteOns...
                      Nothing -> -- error "getNoteEvents: NoteOff without corresponding NoteOn"
                               process m ks
    
    -- returns a list of the noteon and noteoff events in a midi track.
    -- the boolean flag indicates a noteon.
    simplify :: Midi.Track a -> [(a , Bool , Int)]
    simplify = map convert . filter (isNoteEvent . snd)
      where
        convert (x , Midi.NoteOn  _ key v) = (x , v > 0 , key)
        convert (x , Midi.NoteOff _ key _) = (x , False , key)
 
-- VCM: TODO: We will not record tempo changes here; only time signature
--            changes; it should not be too hard to implement tempo changes
--            later.

data Step2State = Step2State
  { ticksPerBeat   :: Midi.Ticks
  , timesigChanges :: M.Map Midi.Ticks TimeSig
  } deriving (Eq , Show)

type Step2 = State Step2State

-- |Get the time signatures of a track and organize them in a map.
midiGetTimeSigs :: Midi.Track Midi.Ticks -> M.Map Midi.Ticks TimeSig
midiGetTimeSigs = M.map convert . M.fromList . filter (isTimeSig . snd)
  where
    convert :: Midi.Message -> TimeSig
    convert (Midi.TimeSignature n d _ _) = TimeSig n (2^d)
    convert _ = error "midiGetTimeSigs: not a time signature!! Did you forget to filter?"

-- |Given a Midi file, returns the state we need
--  to process it into sections.
step2PrepareState :: Midi.Midi -> Step2State
step2PrepareState (Midi.Midi _ (Midi.TicksPerBeat tpb) tr)
  = Step2State tpb (M.unions $ map midiGetTimeSigs tr)

-- |Returns the time signature at a certain tick.
step2TimeSigFor :: Midi.Ticks -> Step2 TimeSig
step2TimeSigFor t
  = (maybe err snd . M.lookupLE t) <$> (timesigChanges <$> get)
  where
    -- err = error "step2TimeSigFor: No time signature found"
    err = TimeSig 4 4

-- |Was there a time signature in between the ticks? This
--  happens when the time signature of two ticks is different.
step2TimeSigChange :: Midi.Ticks -> Midi.Ticks -> Step2 Bool
step2TimeSigChange t t'
  = (/=) <$> step2TimeSigFor t <*> step2TimeSigFor t'

-- |The MIDI ticks-per-beat gives us how many ticks
--  does a quarter note takes.
step2TicksPerMeasure :: Midi.Ticks -> Step2 Midi.Ticks
step2TicksPerMeasure t
  = do (TimeSig n _) <- step2TimeSigFor t
       tpb           <- ticksPerBeat <$> get
       return $ n * tpb

-- |Was there a measure change between these ticks?
--  IMPORTANT: this function does not subtract the ticks
--             from previous sections.
step2MeasureChange :: Midi.Ticks -> Midi.Ticks -> Step2 Bool
step2MeasureChange t t'
  = do tsigChange <- step2TimeSigChange t t'
       if tsigChange
       then return True
       else do tpm <- step2TicksPerMeasure t -- its the same for t' since timesig did NOT change
               return (t `div` tpm /= t' `div` tpm)

-- |Monadic variant of 'groubBy'
groupByM :: (Monad m) => (a -> a -> m Bool) -> [a] -> m [[a]]
groupByM cond []     = return []
groupByM cond (x:xs) = go [x] xs
  where
    go acu     []
      = return [reverse acu]
    go (a:acu) (x:xs)
      = do flag <- cond a x 
           if flag
           then go (x:a:acu) xs
           else (reverse (a:acu) :) <$> (go [x] xs)

-- |Return the sections of a midi file. The outer list has the same
--  length as the number of tracks in the midi file.
midiGetSections :: Midi.Midi -> [[Section]]
midiGetSections midi
  = filter (/= [])
  $ evalState (go $ Midi.tracks midi) (step2PrepareState midi)
  where
    go :: [Midi.Track Midi.Ticks] -> Step2 [[Section]]
    go = mapM (\tr -> splitTS (midiGetNoteEvents tr) >>= mapM splitMS)

    -- Group by time signature block
    splitTS :: [NoteEvent] -> Step2 [[NoteEvent]]
    splitTS = groupByM (\(t0 , _ , _) (t1 , _ , _) -> not <$> step2TimeSigChange t0 t1)

    -- Group by measure
    splitMS :: [NoteEvent] -> Step2 Section
    splitMS []     = error "Can't split an empty section"
    splitMS (x:xs)
      = do tpb <- ticksPerBeat <$> get
           ts  <- step2TimeSigFor (offset x)
           -- XXX: BUG:
           -- VCM: This is not that simple; we have to subtract
           --      the amount of ticks on the previous groups to get
           --      an accurate version
           ms  <- groupByM (\(t0 , _ , _) (t1 , _ , _) -> not <$> step2MeasureChange t0 t1) (x:xs)
           return (Section ts tpb
                  $ map (Measure . map mkNote) ms)

    -- translates a note
    mkNote :: NoteEvent -> Note
    mkNote (offset , dur , pitch) = Note offset dur pitch
    
-- |Given a MIDI file, returns the list of 'Voice's in it
--  or an error.
fromMidi :: FilePath -> IO (Either String [Voice])
fromMidi file
  = do res <- Midi.importFile file
       case res of
         Left err -> return (Left err)
         Right midi -> return $ 
           if not (midiIsMultiTrack midi || midiIsTicksPerBeat midi)
           then Left "Midi is not multi-track or not ticks-per-beat"
           else Right . map Voice $ midiGetSections (midiAbsoluteOffsets midi)

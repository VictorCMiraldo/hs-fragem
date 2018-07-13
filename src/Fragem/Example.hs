module Fragem.Example where

import Fragem.Syntax
import Fragem.Syntax.FromMidi
import Fragem.Syntax.Examples

import Fragem.Metrics
import Fragem.Analisys

testfile  = "dataset/sample/sample2.mid"
testfile2 = "dataset/sample/sample3.mid"
hanonFile = "dataset/hanon/Hanon 1.mid"

-- * Example 1

-- Shows the fractal dimension between two zoom levels
-- of a select number of measures grouped n-by-n from Hanon's
-- first Etude
example1 :: (Int , Int) -- Zoom Levels
         -> Int         -- Number of measures
         -> Int         -- grouping 
         -> IO [Double]
example1 (zoomA , zoomB) nmeasures groupsize
  = do -- file <- fromMidi "dataset/hanon/Hanon 1.mid"
       file <- fromMidi "dataset/jsbach/cellosui/01prelud.mid"
       case file of
         Left err          -> putStrLn ("Error: " ++ err)
                           >> return []
         -- This hanon etude has three 'midi' sections; which
         -- we parse into 'voices'. The first contains information
         -- about time signature and tempo; the second is the treble
         -- cleff and the third is the bass cleff.
         --
         -- In our case, Hanon 1st has a single section.
         -- A section is a sequecne of measures in a tempo/time-sig.
         Right (_:Voice [music]:_)
           -> do let selection = extractMeasures nmeasures music
                 let metric    = metricBeat (sectionSignature music)
                                            (sectionTPB music)
                                            selection
                 let notesA    = regroupMeasure groupsize
                               $ zoomAt zoomA metric selection
                 let notesB    = regroupMeasure groupsize
                               $ zoomAt zoomB metric selection
                 let res = zipWith dimPitches notesA notesB
                 mapM_ (putStrLn . show) res
                 putStrLn "---------------"
                 return res
-- Interstingly; note how
--
--  > example1 (4,3) 10 2
--  >  [0.536537431234617
--  >  ,0.5364619125678102
--  >  ,0.5364523997415686
--  >  ,0.5364619125678102
--  >  ,0.5364523997415686]
--
-- which tells us that looking at strong beats (zoom 4)
-- then strong and middle beats (zoom 3) shows that measures 3 and 4
-- are the same as 7 and 8 (both have fractal dimension 0.5364619125678102).
--
-- This is in fact true, if we look at the interval between the selected
-- notes in the sheet music.
--
-- However; looking further shows that these measures are actually different.
-- For instance, measure 4 goes F A B ... whereas measure 8 goes C E F ...
-- The difference in semitones is 4,2 against 4,1. Hence, at a deeper zoom
-- we see the difference. In fact, if we run the example for a deeper zoom
-- we also notice different fractal dimensions:
--
-- > example1 (3,2) 10 2
-- >   [0.2083862688101164
-- >   ,0.20834906761359967
-- >   ,0.20830055738900566
-- >   ,0.20838593137201267
-- >   ,0.20830055738900566]
--
-- ATTENTION! Zoom 4 has LESS detail than zoom 2!!!

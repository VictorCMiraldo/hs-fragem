module Fragem.Syntax where

import Text.Printf
import Debug.Trace

-- |An 'Offset' is an absolute amount of ticks counting
--  from the start of the piece.
type Offset = Int

-- |Ticks is the unit of time used throughout MIDI
type Ticks  = Int

-- |A 'Note' has a duration, a pitch and it might be tied
--  to the note that comes after.
data Note = Note
  -- | Absolute offset of note from start of the piece; in ticks
  { noteDelay    :: Offset
  -- | Duration of a note
  , noteDuration :: Ticks
  -- | Pitch of the note (or rest)
  , notePitch    :: Int
  } deriving (Eq , Ord)

instance Show Note where
  show (Note nd ndur p) = show nd ++ "+" ++ show ndur ++ ";" ++ show p

-- |Musical time signature.
data TimeSig = TimeSig
  { timesigNum   :: Int
  , timesigDenum :: Int
  } deriving (Eq , Show , Ord)

-- |A 'Measure' consists in a list of notes.
newtype Measure = Measure { measureNotes :: [Note] }
  deriving (Eq , Show , Ord)

-- |Concatenate the notes of n measures into a
--  single group of notes; useful for computing the mass
--  over intervals that are bigger than a measure.
regroupMeasure :: Int -> [Measure] -> [[Note]]
regroupMeasure n ls
  | length ls < n = []
  | otherwise
  = let ls0 = take n ls
        ls1 = drop n ls
     in concat (map measureNotes ls0) : regroupMeasure n ls1

-- |Same as 'regroupMeasure', but employs a sliding
--  window technique.
regroupMeasureSlide :: Int -> [Measure] -> [[Note]]
regroupMeasureSlide n ls
  | length ls < n = []
  | otherwise
  = let ls0 = take n ls
        ls1 = tail ls
        in concat (map measureNotes ls0) : regroupMeasureSlide n ls1

-- |Extract a given number of measures from a section
extractMeasures :: Int -> Section -> [Measure]
extractMeasures i = take i . sectionMeasures


-- |A 'Section' consists in a a list of measures in a given time signature.
data Section = Section
  -- |Current section's time signature
  { sectionSignature :: TimeSig
  -- |Current section's ticks-per-beat
  , sectionTPB       :: Int
  -- |Measures in the given tempo
  , sectionMeasures  :: [Measure]
  } deriving (Eq , Show , Ord)

-- |A 'Voice' is a list of sections.
newtype Voice = Voice { pieceSections :: [Section] }
  deriving (Eq , Show , Ord)

-- |Returns the meta-information of a piece
--  in a humam-consumable format.
metainfoVoices :: [Voice] -> String
metainfoVoices = unlines . concatMap ((["Voice: "] ++) . go)
  where
    ident n = map (replicate n ' ' ++)
    
    go :: Voice -> [String]
    go (Voice ss) = concatMap (([replicate 15 '-'] ++) . goS) ss

    goS :: Section -> [String]
    goS (Section sig tpb ms) = ident 2 $
      [ "timesig:         " ++ show (timesigNum sig)
                            ++ "/" ++ show (timesigDenum sig)
      , "ticks-per-beat:  " ++ show tpb
      , "measures length: " ++ show (length ms)
      ]

-- |Pretty prints a series of notes. The integer parameters
--  tells how many ticks a char corresponds to.
--  Normally, @ticks-per-beat@ and @8@ is a good choice.
prettyNotes :: Int -> Int -> [Note] -> String
prettyNotes tpb cc []
  = "   | EMPTY"
prettyNotes tpb cc ns
  = unlines . on 0   ((printf "%2d |" maxY ++) . drop 4)
            . onLast ((printf "%2d |" minY ++) . drop 4)
            $ map ("   |" ++) (markBars $ map go $ reverse [minY .. maxY])
  where
    (maxY , minY) = foldr (\n (yM , ym) -> (notePitch n `max` yM , notePitch n `min` ym))
                    (0 , 300) ns

    tpc = tpb `div` cc

    markBars :: [String] -> [String]
    markBars lines = let m = maximum (map length lines)
                      in map (complete m) lines

    
    complete m []
      | m > 0 = replicate cc ' ' ++ "|" ++ complete (m - cc) []
      | otherwise = []
    complete m l
      | length l <= cc = l ++ replicate (cc - length l) ' ' ++ "|" ++ complete (m - cc) []
      | otherwise      = take cc l ++ "|" ++ complete (m - cc) (drop cc l)

    on 0 f (x:xs) = f x : xs
    on i f (x:xs) = x : on (i-1) f xs

    onLast f xs   = on (length xs - 1) f xs

    relative (n:ns) = n { noteDelay = 0}
                    : map (\x -> x { noteDelay = noteDelay x - noteDelay n }) ns
 
    ns' = relative ns
   
    go :: Int -> String
    go line = let xs = map (\n -> (noteDelay n `div` tpc
                                  ,noteDuration n `div` tpc))
                     $ filter ((== line) . notePitch) ns'
               in case xs of
                    []       -> ""
                    (ys:yss) -> render 0 (ys:yss)

    render :: Offset -> [(Offset , Ticks)] -> String
    render acu [] = ""
    render acu ((o , t):rest)
      =  replicate (o - acu) ' '
      ++ replicate t '-'
      ++ render (o + t) rest

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main (main) where

import Control.Monad.Except
import Data.Maybe (isJust, fromJust)
import Data.List (partition, isSuffixOf, nub, transpose)
import Data.Char (toLower)
import Text.Printf

import Development.GitRev
import System.Console.CmdArgs.Implicit

import Fragem.Syntax
import Fragem.Syntax.FromMidi
import Fragem.Syntax.ToMidi
import Fragem.Metrics
import Fragem.Analisys
import Fragem.Math.Integral

data Metrics
  = BeatM
  | InnerM
  deriving (Eq , Show , Enum , Data , Typeable)

instance Default Metrics where
  def = BeatM

deriving instance Data FrustumMassType
deriving instance Typeable FrustumMassType

data Options = Options
  { optMetric          :: Metrics
  , optFile            :: Maybe FilePath
  , optVoices          :: [Int]
  , optFrustumMassType :: FrustumMassType
  , optMeasureGroup    :: Int
  , optZoom            :: (Int , Int)
  , optInterval        :: Maybe (Int , Int)
  , optSlide           :: Bool
  , optInfoOnly        :: Bool
  , optPrintHeader     :: Bool
  , optThreshold       :: Maybe Double
  , optExportPats      :: Maybe FilePath
  , optDebug           :: Bool
  } deriving (Eq , Show , Data , Typeable)

options :: Options
options = Options
  { optMetric = BeatM
      &= explicit
      &= name "metric" &= name "m"
      &= typ "METRIC"
      &= help ("which metric to use when zooming in and out.\n"
            ++ "Supported metrics (default: beatm): \n"
            ++ unlines (map (("  - " ++) . map toLower . show) (enumFrom BeatM)))
  , optFile = def
      &= typFile
      &= args
  , optFrustumMassType = 
      enum [ FMT_Volume  &= explicit &= name "fmv-volume"
                         &= help "Use volume computation for polyphony"
           , FMT_Surface &= explicit &= name "fmv-surface"
                         &= help "Use surface computation for polyphony"
           ]
  , optExportPats = Nothing
      &= name "e" &= name "export"
      &= typFile
      &= help ("Export the patterns we find as midi files. Will add\n"
             ++"a number before the mid extension indicating the group this belongs to")
  , optThreshold = Nothing
      &= name "t" &= name "threshold"
      &= typ "FLOAT"
      &= help "group measures that have dimensions in a given threshold"
  , optVoices =
      enum [ [0]     &= name "c0"
           , [1]     &= name "c1"
           , [2]     &= name "c2"
           , [3]     &= name "c3"
           , [4]     &= name "c4"
           , [5]     &= name "c5"
           , [6]     &= name "c6"
           ] &= help "Select a given voice for analisys"
  , optMeasureGroup = 2
      &= explicit
      &= name "group" &= name "g"
      &= typ "INT"
      &= help "How many measures should make up a group (min: 2)"
  , optZoom = (2,1)
      &= explicit
      &= name "zoom" &= name "z"
      &= typ "INT,INT"
      &= help "which zoom levels to consider"
  , optSlide = def
      &= explicit
      &= name "slide" &= name "s"
      &= typ "BOOL"
      &= help "Use a sliding window"
  , optInfoOnly = def
      &= explicit
      &= name "info-only" 
      &= typ "BOOL"
      &= help "Only prints information about the midi file. Implies -v"
  , optInterval = Nothing
      &= explicit
      &= name "i" &= name "interval"
      &= typ "INT,INT"
      &= help "If present, consider only the selected interval of measures"
  , optPrintHeader = def
      &= explicit
      &= name "header" &= name "h"
      &= typ "BOOL"
      &= help "Prints header about interval, grouping and sliding"
  , optDebug = def
      &= explicit
      &= name "debug" &= name "d"
      &= typ "BOOL"
      &= help "Turns on debugging information"
  } &= summary ("v0.0.0 [" ++ $(gitBranch) ++ "@" ++ $(gitHash) ++ "]")
    &= program "fragem"
    &= verbosity
    &= details
       [ "Returns a list of fractal dimensions of the measures in a"
       , "MIDI file."
       ]
      
main :: IO ()
main = cmdArgs options
   >>= runExceptT . go
   >>= \res -> case res of
                 Left err -> putStrLn $ "!! " ++ err
                 Right ds -> return ()

printDimensions :: [(Int , Double)] -> IO ()
printDimensions = mapM_ (\(ms , dim) -> putStrLn $ printf "%3d %.12f" ms dim)

type M = ExceptT String IO

errWhen :: (MonadError e m) => Bool -> e -> m ()
errWhen cond e = when cond $ throwError e

warnWhen :: Bool -> String -> M ()
warnWhen True  = lift . putStrLn . ("** " ++)
warnWhen False = const (return ())

go :: Options -> M ()
go opts = do
  errWhen (not . isJust $ optFile opts)
          "No file provided"
  midi <- ExceptT $ fromMidi (fromJust $ optFile opts) 
  lift $ when (optInfoOnly opts) $ setVerbosity Loud
  lift $ whenLoud (printMidiInfo midi)
  if (optInfoOnly opts)
    then return ()
    else do
      let selectVoices = nub (optVoices opts)
      errWhen (all (length midi <=) $ selectVoices)
              "Not enough voices"
      let voices      = map (midi !!) $ selectVoices
      errWhen (voices == [])
              "No voices selected. Nothing to analyze"
      warnWhen (any ((> 1) . length . pieceSections) voices)
               "Some voices have multiple sections; use -d to figure out which"
      let measureStart = maybe 0 fst $ optInterval opts
      let measureIdxs = if optSlide opts
                        then [measureStart ..]
                        else [measureStart , measureStart + optMeasureGroup opts ..]
      when (optPrintHeader opts)
        $ lift $ printHeader opts
      -- compute the dimensions of the specified parts
      let sects = map (head . pieceSections) voices
      dims <- zip measureIdxs <$> (lift $ dimensions opts sects)
      if (isJust $ optThreshold opts)
      then thresholdGroup opts (fromJust $ optThreshold opts) dims sects
      else lift $ printDimensions dims

thresholdGroup :: Options -> Double -> [(Int , Double)] -> [Section] -> M ()
thresholdGroup opts thre dims sects
  = do let groups = makeGroups dims
       lift $ mapM_ printGroup groups
       when (isJust (optExportPats opts))
         $ exportGroups opts (map (map fst) groups) sects
  where
    makeGroups [] = []
    makeGroups ((n , d):dims)
      = let (ing , outg) = partition (\(n' , d') -> abs (d - d') <= thre) dims
         in ((n , d):ing) : makeGroups outg

    printGroup :: [(Int , Double)] -> IO ()
    printGroup gs
      = let (ns , ds) = unzip gs
         in putStrLn $ printf "~%.12f %s" (average ds) (unwords $ map show ns)

    average :: [Double] -> Double
    average ns = sum ns / fromIntegral (length ns)

exportGroups :: Options -> [[Int]] -> [Section] -> M ()
exportGroups opts gs sects
  = lift $ flip mapM_ (zip [0..] gs) $ \(patN , msIdx)
  -> let file = format patN (fromJust $ optExportPats opts)
      in sectionsToMidi file (map (select msIdx) sects) 
  where
    select :: [Int] -> Section -> Section
    select ns s = s { sectionMeasures = concat $ map (\ idx
               -> take (optMeasureGroup opts) (drop idx $ sectionMeasures s)) ns }

    format n str
      | ".mid" `isSuffixOf` str
      = take (length str - 4) str ++ "." ++ show n ++ ".mid"
      | otherwise
      = str ++ "." ++ show n ++ ".mid"

printHeader :: Options -> IO ()
printHeader opts
  =  putStrLn ("> measuregroup: " ++ show (optMeasureGroup opts))
  >> putStrLn ("> sliding: "      ++ show (optSlide opts))
  >> putStrLn ("> interval: "     ++ show (optInterval opts))
  

printMidiInfo :: [Voice] -> IO ()
printMidiInfo voices
  =  putStrLn "Midi Information: "
  >> putStrLn (metainfoVoices voices)

{-
dimensions :: Options -> [Section] -> IO [Double]
dimensions opts sections@(section:_) = do
  let (zA , zB) = optZoom opts
  let zoomB = max zA zB
  let zoomA = min zA zB
  let sects@(sect:_) = map sectionMeasures sections
  let mF = case optMetric opts of
             BeatM  -> metricBeat (sectionSignature section)
                                  (sectionTPB section)
             InnerM -> metricInner (optMeasureGroup opts)
  let metric = mF sect
  let mS = if optSlide opts
           then regroupMeasureSlide
           else regroupMeasure
  let sects' = case optInterval opts of
                 Nothing            -> sects
                 Just (start , end) -> map (take (end - start + 1) . drop (start - 1)) sects
  let notesA = map ( mS (optMeasureGroup opts)
                   . zoomAt zoomA metric) sects'
  let notesB = map (mS (optMeasureGroup opts)
                   . zoomAt zoomB metric) sects'
  _
  putStrLn "NotesA: "
  putStrLn ("> " ++ show notesA)
  when (optDebug opts) $
    do putStrLn ("max metric zoom level: " ++ show (maximum $ metricLevels metric))
       putStrLn ("Mass: "     ++ show (notesMass (optFrustumMassType opts) $ concat notesA))
       putStrLn ("Mass: "     ++ show (notesMass (optFrustumMassType opts) $ concat notesB))
       mapM_ (uncurry debugInfoFor) (zip notesA notesB)
  return $ zipWith (dimPitches (optFrustumMassType opts)) notesA notesB
 where
  debugInfoFor notesA notesB = do
    let nA = prettyNotes (sectionTPB section) 8 $ concat notesA
        nB = prettyNotes (sectionTPB section) 8 $ concat notesB
        res = if length nA >= length nB
              then zipWith (\a b -> a ++ "  |  " ++ b)
                           nA (nB ++ repeat " ")
              else zipWith (\a b -> a ++ "  |  " ++ b)
                           (nA ++ repeat (replicate (length $ head nA) ' ')) nB
    putStrLn (unlines res)

-}

-- Make sure that the selectec sections satisfy a bunch of preconditions.
-- If they do, we return their notes and the metainfo.
dimensionsPrecond :: Options -> [Section] -> IO (Maybe (TimeSig, Int, [[Measure]]))
dimensionsPrecond opts sections = do
  let tpbs = map sectionTPB sections
  let tss  = map sectionSignature sections
  let conds = filter (not . fst)
            $ [ (length (nub tpbs) == 1
                , "ticks-per-beat between selected voices disagree")
              , (length (nub tss) == 1
                , "time signatures between selected voices disgree")
              ]
  case conds of
    [] -> return (Just (head tss , head tpbs , map sectionMeasures sections))
    _  -> mapM (putStrLn . snd) conds
       >> return Nothing
                
dimensions :: Options -> [Section] -> IO [Double]
dimensions opts sections =
  dimensionsPrecond opts sections >>= maybe (return []) analyze
  where
    -- Zoom level
    (zA , zB) = optZoom opts
    zoomA     = min zA zB
    zoomB     = max zA zB

    -- which volume computation to use
    fmv       = optFrustumMassType opts

    -- The regrouping function PER-VOICE
    mS        :: [Measure] -> [[Note]]
    mS        = if optSlide opts
                then regroupMeasureSlide (optMeasureGroup opts)
                else regroupMeasure      (optMeasureGroup opts)

    -- The interval selection function, PER-VOICE
    mCut      :: [Measure] -> [Measure]
    mCut      = case optInterval opts of
                  Nothing            -> id
                  Just (start , end) -> take (end - start + 1) . drop (start - 1)

    -- The metric function
    mF        :: TimeSig -> Int -> [Measure] -> Metric
    mF ts tpb = case optMetric opts of
                  BeatM  -> metricBeat ts tpb
                  InnerM -> metricInner (optMeasureGroup opts)

    
    analyze :: (TimeSig, Int, [[Measure]]) -> IO [Double]
    analyze (ts , tpb , mss) = do
      -- select the required measures for each voice
      let sects  = map mCut mss 
      -- zoom them in and out on the correct zoom levels
      let sectsA = map (\m -> mS $ zoomAt zoomA (mF ts tpb m) m) sects
      let sectsB = map (\m -> mS $ zoomAt zoomB (mF ts tpb m) m) sects
      mapM_ (putStrLn . show) sectsA
      when (optDebug opts)
        $ do putStrLn ("Notes at: " ++ show zoomA)
             putStrLn ("Mass: "     ++ show (notesMass fmv $ map concat sectsA))
             putStrLn ("Notes at: " ++ show zoomB)
             putStrLn ("Mass: "     ++ show (notesMass fmv $ map concat sectsB))
             let nA = prettyNotes tpb 8 $ map concat sectsA
                 nB = prettyNotes tpb 8 $ map concat sectsB
                 res = if length nA >= length nB
                       then zipWith (\a b -> a ++ "  |  " ++ b)
                                    nA (nB ++ repeat " ")
                       else zipWith (\a b -> a ++ "  |  " ++ b)
                                    (nA ++ repeat (replicate (length $ head nA) ' ')) nB
             putStrLn (unlines res)
      return $ zipWith (dimPitches fmv) (transpose sectsA) (transpose sectsB)


{-
dimensions :: Options -> [Section] -> IO [Double]
dimensions opts (section:_) = do
  let (zA , zB) = optZoom opts
  let zoomB = max zA zB
  let zoomA = min zA zB
  let fmv  = optFrustumMassType opts
  let sect = sectionMeasures section
  let mF = case optMetric opts of
             BeatM  -> metricBeat (sectionSignature section)
                                  (sectionTPB section)
             InnerM -> metricInner (optMeasureGroup opts)
  let metric = mF sect
  let mS = if optSlide opts
           then regroupMeasureSlide
           else regroupMeasure
  let sect' = case optInterval opts of
                Nothing            -> sect
                Just (start , end) -> take (end - start + 1) $ drop (start - 1) sect
  let notesA = mS (optMeasureGroup opts)
             $ zoomAt zoomA metric sect'
  let notesB = mS (optMeasureGroup opts)
             $ zoomAt zoomB metric sect'
  when (optDebug opts)
    $ do putStrLn ("max metric zoom level: " ++ show (maximum $ metricLevels metric))
         putStrLn ("Notes at: " ++ show zoomA)
         putStrLn ("Mass: "     ++ show (notesMass fmv $ [concat notesA]))
         putStrLn ("Notes at: " ++ show zoomB)
         putStrLn ("Mass: "     ++ show (notesMass fmv $ [concat notesB]))
         let nA = prettyNotes (sectionTPB section) 8 $ concat notesA
             nB = prettyNotes (sectionTPB section) 8 $ concat notesB
             res = if length nA >= length nB
                   then zipWith (\a b -> a ++ "  |  " ++ b)
                                nA (nB ++ repeat " ")
                   else zipWith (\a b -> a ++ "  |  " ++ b)
                                (nA ++ repeat (replicate (length $ head nA) ' ')) nB
         putStrLn (unlines res)
  return $ zipWith (\na nb -> dimPitches fmv [na] [nb]) notesA notesB
-}

{-
go :: Options -> M ()
go opts = do
  errWhen (not . isJust $ optFile opts)
          "No file provided"
  midi <- ExceptT $ fromMidi (fromJust $ optFile opts) 
  lift $ when (optInfoOnly opts) $ setVerbosity Loud
  lift $ whenLoud (printMidiInfo midi)
  if (optInfoOnly opts)
    then return ()
    else do
      errWhen (length midi <= optVoice opts)
              "Not enough voices"
      let Voice voice = midi !! (optVoice opts)
      errWhen (voice == [])
              "Voice is empty, nothing to analyze"
      warnWhen (length voice > 1)
               "This voice has multiple sections, we will only look at the first."
      let measureStart = maybe 0 fst $ optInterval opts
      let measureIdxs = if optSlide opts
                        then [measureStart ..]
                        else [measureStart , measureStart + optMeasureGroup opts ..]
      when (optPrintHeader opts)
        $ lift $ printHeader opts
      -- compute the dimensions of the specified parts
      let sect = head voice
      dims <- zip measureIdxs <$> (lift $ dimensions opts sect)
      if (isJust $ optThreshold opts)
      then thresholdGroup opts (fromJust $ optThreshold opts) dims sect
      else lift $ printDimensions dims

thresholdGroup :: Options -> Double -> [(Int , Double)] -> Section -> M ()
thresholdGroup opts thre dims sect
  = do let groups = makeGroups dims
       lift $ mapM_ printGroup groups
       when (isJust (optExportPats opts))
         $ exportGroups opts (map (map fst) groups) sect
  where
    makeGroups [] = []
    makeGroups ((n , d):dims)
      = let (ing , outg) = partition (\(n' , d') -> abs (d - d') <= thre) dims
         in ((n , d):ing) : makeGroups outg

    printGroup :: [(Int , Double)] -> IO ()
    printGroup gs
      = let (ns , ds) = unzip gs
         in putStrLn $ printf "~%.12f %s" (average ds) (unwords $ map show ns)

    average :: [Double] -> Double
    average ns = sum ns / fromIntegral (length ns)

exportGroups :: Options -> [[Int]] -> Section -> M ()
exportGroups opts gs sect
  = lift $ flip mapM_ (zip [0..] gs) $ \(patN , msIdx)
  -> let ms = concatMap fetch msIdx
         file = format patN (fromJust $ optExportPats opts)
      in sectionToMidi file (sect { sectionMeasures = ms })
  where
    fetch :: Int -> [Measure]
    fetch n = take (optMeasureGroup opts) (drop n $ sectionMeasures sect)

    format n str
      | ".mid" `isSuffixOf` str
      = take (length str - 4) str ++ "." ++ show n ++ ".mid"
      | otherwise
      = str ++ "." ++ show n ++ ".mid"

printHeader :: Options -> IO ()
printHeader opts
  =  putStrLn ("> measuregroup: " ++ show (optMeasureGroup opts))
  >> putStrLn ("> sliding: "      ++ show (optSlide opts))
  >> putStrLn ("> interval: "     ++ show (optInterval opts))
  

printMidiInfo :: [Voice] -> IO ()
printMidiInfo voices
  =  putStrLn "Midi Information: "
  >> putStrLn (metainfoVoices voices)

-}

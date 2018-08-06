{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad.Except
import Data.Maybe (isJust, fromJust)
import Data.Char (toLower)
import Text.Printf

import Development.GitRev
import System.Console.CmdArgs.Implicit

import Fragem.Syntax
import Fragem.Syntax.FromMidi
import Fragem.Metrics
import Fragem.Analisys

data Metrics
  = BeatM
  | InnerM
  deriving (Eq , Show , Enum , Data , Typeable)

instance Default Metrics where
  def = BeatM

data Options = Options
  { optMetric       :: Metrics
  , optFile         :: Maybe FilePath
  , optVoice        :: Int
  , optMeasureGroup :: Int
  , optZoom         :: (Int , Int)
  , optInterval     :: Maybe (Int , Int)
  , optSlide        :: Bool
  , optInfoOnly     :: Bool
  , optDebug        :: Bool
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
  , optVoice = 0
      &= explicit
      &= name "voice" 
      &= typ "INT"
      &= help "which midi track to use"
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
                 Right ds -> mapM_ (putStrLn . printf "%.12f") ds

type M = ExceptT String IO

errWhen :: (MonadError e m) => Bool -> e -> m ()
errWhen cond e = when cond $ throwError e

warnWhen :: Bool -> String -> M ()
warnWhen True  = lift . putStrLn . ("** " ++)
warnWhen False = const (return ())

go :: Options -> M [Double]
go opts = do
  errWhen (not . isJust $ optFile opts)
          "No file provided"
  midi <- ExceptT $ fromMidi (fromJust $ optFile opts) 
  lift $ when (optInfoOnly opts) $ setVerbosity Loud
  lift $ whenLoud (printMidiInfo midi)
  if (optInfoOnly opts)
    then return []
    else do
      errWhen (length midi <= optVoice opts)
              "Not enough voices"
      let Voice voice = midi !! (optVoice opts)
      errWhen (voice == [])
              "Voice is empty, nothing to analyze"
      warnWhen (length voice > 1)
               "This voice has multiple sections, we will only look at the first."
      lift $ runAnalisys opts (head voice)

printMidiInfo :: [Voice] -> IO ()
printMidiInfo voices
  =  putStrLn "Midi Information: "
  >> putStrLn (metainfoVoices voices)

runAnalisys :: Options -> Section -> IO [Double]
runAnalisys opts section = do
  let (zA , zB) = optZoom opts
  let zoomA = max zA zB
  let zoomB = min zA zB
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
         putStrLn (prettyNotes (sectionTPB section) 8 $ concat notesA)
         putStrLn ("Notes at: " ++ show zoomB)
         putStrLn (prettyNotes (sectionTPB section) 8 $ concat notesB)

  return $ zipWith dimPitches notesA notesB

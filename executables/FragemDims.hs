{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad.Except
import Data.Maybe (isJust, fromJust)

import Development.GitRev
import System.Console.CmdArgs.Implicit

import Fragem.Syntax
import Fragem.Syntax.FromMidi
import Fragem.Metrics
import Fragem.Analisys

data Metrics
  = BeatM
  | InnerM
  deriving (Eq , Show , Data , Typeable)

instance Default Metrics where
  def = BeatM

data Options = Options
  { optMetric       :: Metrics
  , optFile         :: Maybe FilePath
  , optVoice        :: Int
  , optMeasureGroup :: Int
  , optZoom         :: (Int , Int)
  , optSlide        :: Bool
  } deriving (Eq , Show , Data , Typeable)

options :: Options
options = Options
  { optMetric = def
      &= explicit
      &= name "metric"
      &= opt BeatM
      &= typ "METRIC"
      &= help "which metric to use when zooming in and out."
  , optFile = def
      &= explicit
      &= name "file"
      &= typ "FILE"
      &= help "input midi file"
  , optVoice = 1
      &= explicit
      &= name "voice"
      &= typ "INT"
      &= help "which midi track to use"
  , optMeasureGroup = 2
      &= explicit
      &= name "group"
      &= typ "INT"
      &= help "How many measures should make up a group (min: 2)"
  , optZoom = (2,1)
      &= explicit
      &= name "zoom"
      &= typ "INT,INT"
      &= help "which zoom levels to consider"
  , optSlide = def
      &= explicit
      &= name "slide"
      &= typ "BOOL"
      &= help "Use a sliding window"
  } &= help "fragem-dims v0.0.0"
    &= summary ("fragem-dims [" ++ $(gitBranch) ++ "@" ++ $(gitHash) ++ "]")
    &= details
       [ "Returns a list of fractal dimensions of the measures in a"
       , "MIDI file"
       ]
      
main :: IO ()
main = cmdArgs options
   >>= runExceptT . go
   >>= \res -> case res of
                 Left err -> putStrLn $ "!! " ++ err
                 Right ds -> mapM_ (putStrLn . show) ds

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
  errWhen (length midi <= optVoice opts)
          "Not enough voices"
  let Voice voice = midi !! (optVoice opts)
  errWhen (voice == [])
          "Voice is empty, nothing to analyze"
  warnWhen (length voice > 1)
           "This voice has multiple sections, we will only look at the first."
  return $ runAnalisys opts (head voice)

runAnalisys :: Options -> Section -> [Double]
runAnalisys opts section =
  let (zoomA , zoomB) = optZoom opts
      sect = sectionMeasures section
      mF = case optMetric opts of
             BeatM  -> metricBeat (sectionSignature section)
                                  (sectionTPB section)
             InnerM -> metricInner
      mS = if optSlide opts
           then regroupMeasureSlide
           else regroupMeasure
      notesA = mS (optMeasureGroup opts)
             $ zoomAt zoomA (mF sect) sect
      notesB = mS (optMeasureGroup opts)
             $ zoomAt zoomB (mF sect) sect
   in zipWith dimPitches notesA notesB

module Fragem.Syntax.FromMidiSpec (spec) where

import Fragem.Syntax.FromMidi

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

spec :: Spec
spec = do
  describe "fromMidi" $ do
    it "parses Hanon #1 with no errors" $ monadicIO $
      do res <- run $ fromMidi "dataset/hanon/Hanon 1.mid"
         case res of
           Left err -> return False
           Right _  -> return True

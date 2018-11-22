module Fragem.AnalisysSpec (spec) where

import Fragem.Syntax
import Fragem.Analisys
import Fragem.Math.Integral

import Test.QuickCheck
import Test.Hspec

notesMassLocal :: [Note] -> Maybe Double
notesMassLocal = notesMass FMT_Volume . (:[])

spec :: Spec
spec = do
  describe "notesMassLocal" $ do
    it "Computes examples correctly" $
      (notesMassLocal [Note 0 40 60 , Note 40 40 120] `shouldBe` (Just 140.0))
        .&&. (notesMassLocal [ Note 0 40 40 , Note 40 40 90 , Note 100 20 95]
               `shouldBe` (Just 170.6155281280883))

    -- it does not fail now, in fact, we always return Just;
    -- TODO: remove Maybe from 'notesMass' type.
    xit "Fails on inputs with length less than 2" $ property $
      notesMassLocal [] == Nothing .&&. notesMassLocal [Note undefined undefined undefined] == Nothing
  

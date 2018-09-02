module Fragem.AnalisysSpec (spec) where

import Fragem.Syntax
import Fragem.Analisys

import Test.QuickCheck
import Test.Hspec

spec :: Spec
spec = do
  describe "notesMass" $ do
    it "Computes examples correctly" $
      notesMass [Note 0 40 60 , Note 40 40 120] `shouldBe` (Just 140.0)

    it "Fails on inputs with length less than 2" $ property $
      notesMass [] == Nothing .&&. notesMass [Note undefined undefined undefined] == Nothing
  

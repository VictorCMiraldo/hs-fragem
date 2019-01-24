module Fragem.AnalisysSpec (spec) where

import Fragem.Syntax
import Fragem.Metrics

import Test.QuickCheck
import Test.Hspec

spec :: Spec
spec = do
  describe "metricBeat" $ do
    it "Beat 0" $
      metricBeat (TimeSig 3 4) 4 [] `shouldBe` []

    --it "IMA Example" $ property $
      -- notesMass [] == Nothing .&&. notesMass [Note undefined undefined undefined] == Nothing

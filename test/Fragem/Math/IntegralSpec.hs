module Fragem.Math.IntegralSpec (spec) where

import Fragem.Math.Integral

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

spec :: Spec
spec = do
  describe "areaOfSegment" $ do
    it "try out area of (1,1) and (1,1)" $
      (areaOfSegment [(1,1), (1,1)] `shouldBe` 0)
module Fragem.Math.IntegralSpec (spec) where

import Fragem.Math.Integral

import Data.List

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

genSortedDoubles :: Gen [Double]
genSortedDoubles = do
  d1 <- arbitrary
  d2 <- arbitrary `suchThat` (/= d1)
  nub . sort . ([d1 , d2] ++) <$> listOf arbitrary

genLine :: Gen [Point]
genLine = do
  xs <- genSortedDoubles
  ys <- vectorOf (length xs) arbitrary
  return (zip xs ys)
  
epsilon :: Double
epsilon = 10e-12

spec :: Spec
spec = do
  describe "lengthOfSegment" $ do
    it "computes diagonal of square" $
      lengthOfSegment [(0,0) , (1,1)] `shouldBe` sqrt 2

    it "works for a bigger square" $
      lengthOfSegment [(0,0) , (1,1) , (2,2)] `shouldBe` 2 * (sqrt 2)

    it "works under transpositions" $ property $
      forAll genLine $ \ l -> forAll arbitrary $ \ pt
        -> (lengthOfSegment l - lengthOfSegment (map ((.+.) pt) l)) <= epsilon
      
  describe "completeLineWith" $ do
    it "is idempotent" $ property $
      forAll genLine $ \ l -> completeLineWith (map fst l) l === l

    it "X dimensions is correct" $ property $
      forAll genLine $ \ l -> forAll genSortedDoubles $ \ xs
        -> let lxs = map fst l
            in map fst (completeLineWith xs l)
               === nub (sort (lxs ++ xs))

  describe "frustumVolume" $ do
    it "special case: cube" $ do
      frustumVolume (replicate 4 [(0,1) , (1,1)]) `shouldBe` 1

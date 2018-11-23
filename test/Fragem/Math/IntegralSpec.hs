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

genPositiveDouble :: Gen Double
genPositiveDouble = choose (1e-4 , 1e2) 

genLine :: Gen [Point]
genLine = do
  xs <- genSortedDoubles
  ys <- vectorOf (length xs) arbitrary
  return (zip xs ys)
  
epsilon :: Double
epsilon = 1e-2

-- perp :: [Line] -> Double -> Double -> Bool
-- perp lines expected resolution = (abs(res - expected) <= resolution)
--   where res = frustumVolume frustumSectionVolume lines

spec :: Spec
spec = do
  describe "lengthOfSegment" $ do
    it "computes diagonal of square" $
      lengthOfSegment [(0,0) , (1,1)] `shouldBe` sqrt 2

    it "works for a bigger square" $
      lengthOfSegment [(0,0) , (1,1) , (2,2)] `shouldBe` 2 * (sqrt 2)

    it "works under transpositions" $ property $
      forAll genLine $ \ l -> forAll arbitrary $ \ pt
        -> abs (lengthOfSegment l - lengthOfSegment (map ((.+.) pt) l)) <= epsilon
      
  describe "completeLineWith" $ do
    it "is idempotent" $ property $
      forAll genLine $ \ l -> completeLineWith (map fst l) l === l

    it "X dimensions is correct" $ property $
      forAll genLine $ \ l -> forAll genSortedDoubles $ \ xs
        -> let lxs = map fst l
            in map fst (completeLineWith xs l)
               === nub (sort (lxs ++ xs))

  describe "frustumVolume: volume" $ do
    it "special case: cube" $ property $ forAll genPositiveDouble $
      \ side -> let x    = sqrt ( 2 * side ^ 2) / 2
                    cube = replicate 4 [(0 , x) , (side , x)]
                 in abs (frustumVolume frustumSectionVolume cube - (side ^ 3)) <= epsilon
 
  describe "frustumVolume: surface" $ do
    it "special case: cuboid" $ property $ forAll genPositiveDouble $
      \ x -> let cube = replicate 4 [(0 , x) , (1 , x)]
                 expect = 4 * sqrt (x ** 2 + x ** 2)
                 res = frustumVolume frustumSectionSurface cube 
              in counterexample ("expected: " ++ show expect ++ "; got: " ++ show res)
               $ abs (res - expect) <= epsilon

  describe "frustumVolume: perpendicular" $ do
    it "special case: one perpendicular, one stable" $
      let perp = [[(0,0),(1,0),(1,1),(2,1)],[(0,3),(1,3),(2,3),(3,3)]]
          expect = 10
          res = frustumVolume frustumSectionVolume perp
       in if (abs (res - expect) <= epsilon)
           then True `shouldBe` True
           else expectationFailure ("expected: " ++ show expect ++ "; got: " ++ show res)

 -- how to package the function so that we can still show expect values and the result values? 
 -- describe "frustumVolume: perpendicular" $ do
 --    it "special case: perp1" $
 --      let expect = 10 in
 --        let desicion = perp [[(0,0),(1,0),(1,1),(2,1)],[(0,3),(1,3),(2,3),(3,3)] expect 1 in
 --          if decision
 --            then True `shouldBe` True
 --            else expectationFailure ("expected: " ++ show expect ++ "; got: " ++ show res)

               
  describe "frustumVolume: volume" $ do
    it "special case: four perpendicular" $
      let perp = replicate 4 [(0,0),(1,0),(1,1),(2,1)]
          expect = 2
          res = frustumVolume frustumSectionVolume perp
            in if (abs (res - expect) <= epsilon)
               then True `shouldBe` True
               else expectationFailure ("expected: " ++ show expect ++ "; got: " ++ show res)
               
  describe "frustumVolume: volume" $ do
    it "special case: two perpendicular two stable" $
      let perp = [[(0,0),(1,0),(1,1),(2,1)],[(0,3),(1,3),(2,3),(3,3)],[(0,0),(1,0),(1,1),(2,1)],[(0,3),(1,3),(2,3),(3,3)]]
          expect = 4
          res = frustumVolume frustumSectionVolume perp
            in if (abs (res - expect) <= epsilon)
               then True `shouldBe` True
               else expectationFailure ("expected: " ++ show expect ++ "; got: " ++ show res)

               
  describe "frustumVolume: volume" $ do
    it "special case: longer perpendiculars" $ 
      let perp = replicate 4 [(0,0),(1,0),(1,1),(2,1),(3,0),(4,0),(4,1)]
          expect = 6.6666
          res = frustumVolume frustumSectionVolume perp
       in if (abs (res - expect) <= epsilon)
               then True `shouldBe` True
               else expectationFailure ("expected: " ++ show expect ++ "; got: " ++ show res)

               
  describe "frustumVolume: volume" $ do
    it "special case: shifted perpendiculars" $ 
       let perp = [[(0,0),(1,0),(1,1),(2,1),(3,0),(4,0),(4,1)],[(1,0),(1,1),(2,1),(3,0),(4,0),(4,1)],[(1,1),(2,1),(3,0),(4,0),(4,1)]]
           expect = 0.875
           res = frustumVolume frustumSectionVolume perp
            in if (abs (res - expect) <= epsilon)
               then True `shouldBe` True
               else expectationFailure ("expected: " ++ show expect ++ "; got: " ++ show res)
                    
  describe "frustumVolume: volume" $ do
    it "special case: shifted perpendiculars" $ 
       let perp = [[(0,0),(0.5,0),(0.5,1),(2,1),(3,0),(4,0),(4,1)],[(1,0),(1,1),(2,1),(0.5,0),(4,0),(4,1)],[(1,1),(2,1),(3,0),(4,0),(4,1)]]
           expect = 0.875
           res = frustumVolume frustumSectionVolume perp
            in if (abs (res - expect) <= epsilon)
               then True `shouldBe` True
               else expectationFailure ("expected: " ++ show expect ++ "; got: " ++ show res)

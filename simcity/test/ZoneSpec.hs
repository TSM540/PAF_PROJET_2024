module ZoneSpec where

import Test.Hspec
import Test.QuickCheck

import Zone
import Forme

zonesDisjointesTest1 = zonesDisjointes (Eau (Rectangle (C 0 0) 1 2)) (Route (Rectangle (C 0 0) 10 5))
zonesDisjointesTest2 = zonesDisjointes (Eau (Rectangle (C 0 0) 1 2)) (Eau (Rectangle (C 1 2) 10 5))
zonesDisjointesTest3 = zonesDisjointes (Eau (Rectangle (C 0 0) 1 2)) (Eau (Rectangle (C 2 3) 10 5))

zoneSpec :: Spec
zoneSpec = do

  describe "zoneForme" $ do
    it "extracts shape from water zone" $ do
      zoneForme (Eau (Rectangle (C 0 0) 10 5)) `shouldBe` (Rectangle (C 0 0) 10 5)

  describe "zonesDisjointes" $ do
    it "checks disjointness of zones with overlapping shapes (one inside)" $ do
      zonesDisjointesTest1 `shouldBe` False
    it "checks disjointness of zones with touching shapes (same type)" $ do
      zonesDisjointesTest2 `shouldBe` False
    it "checks disjointness of zones with non-touching shapes (same type)" $ do
      zonesDisjointesTest3 `shouldBe` True



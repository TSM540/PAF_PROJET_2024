module FormeSpec where

import Test.Hspec
import Test.QuickCheck
import Forme

forme1 = HSegement (C 1 2) 5
forme2 = VSegement (C 1 2) 5
forme3 = Rectangle (C 1 2) 5 5 

point1 = C (-1) 1
segment1 = HSegement (C 0 1) 5

point2 = C 0 0
segment2 = VSegement (C 0 1) 5


point3 = C 2 5
rectangle1 = Rectangle (C 0 0) 5 5

f1 = Rectangle (C 0 0) 10 5
f2 = Rectangle (C 5 5) 10 5

f3 = Rectangle (C 0 0) 2 2
f4 = Rectangle (C 4 4) 2 2

f5 = Rectangle (C 0 0) 2 2
f6 = Rectangle (C 2 2) 2 2

formeSpec :: Spec
formeSpec = do

  describe "limites" $ do
    it "calculates limits for HSegement" $ do
      limites (HSegement (C 1 2) 5) `shouldBe` (1, 6, 2, 2)
    it "calculates limits for VSegement" $ do
      limites (VSegement (C 1 2) 5) `shouldBe` (1, 1, 2, 7)
    it "calculates limits for Rectangle" $ do
      limites (Rectangle (C 1 2) 5 5) `shouldBe` (1, 6, 2, 7)

  describe "appartient" $ do
    it "checks point membership in HSegement" $ do
      appartient (C 3 2) (HSegement (C 1 2) 5) ` shouldBe` True
    it "checks point membership in VSegement (negative case)" $ do
      appartient (C 3 2) (VSegement (C 1 2) 5) ` shouldBe` False
    it "checks point membership in Rectangle" $ do
      appartient (C 3 2) (Rectangle (C 1 2) 5 5) ` shouldBe` True

  describe "adjacent" $ do
    it "checks point adjacency to HSegement (negative case)" $ do
      adjacent point1 segment1 ` shouldBe` False
    it "checks point adjacency to VSegement (negative case)" $ do
      adjacent point2 segment2 ` shouldBe` False
    it "checks point adjacency to Rectangle" $ do
      adjacent point3 rectangle1 ` shouldBe` True
    it "checks point adjacency to Rectangle (negative case)" $ do
      adjacent (C 0 0) (Rectangle (C 0 0) 5 5) ` shouldBe` False

  describe "collisionApprox" $ do
    it "checks approximate collision between rectangles" $ do
      collisionApprox f1 f2 ` shouldBe` True
    it "checks no approximate collision between rectangles" $ do
      collisionApprox f3 f4 ` shouldBe` False
    it "checks approximate collision between rectangles (touching)" $ do
      collisionApprox f5 f6 ` shouldBe` True

  describe "collision" $ do
    it "checks exact collision between rectangles" $ do
      collision f1 f2 ` shouldBe` True
    it "checks no exact collision between rectangles" $ do
      collision f3 f4 ` shouldBe` False



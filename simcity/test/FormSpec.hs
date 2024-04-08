module FormeSpec where

import Test.Hspec
import Test.QuickCheck
import Forme

spec :: Spec
spec = do

  describe "appartient" $ do
    it "should return True for a point inside a segment" $ do
      property $ forAll (genCoord) $ \coord ->
        appartient coord (HSegement (C 0 0) 5) ==> (coord `elem` [(C 0, 0), (C 1, 0), (C 2, 0), (C 3, 0), (C 4, 0)])

    it "should return True for a point inside a rectangle" $ do
      property $ forAll (genCoord) $ \coord ->
        appartient coord (Rectangle (C 0 0) 5 5) ==> (coord `elem` [(C 0, 0), (C 1, 0), (C 2, 0), (C 3, 0), (C 4, 0),
                                                                 (C 0, 1), (C 1, 1), (C 2, 1), (C 3, 1), (C 4, 1),
                                                                 (C 0, 2), (C 1, 2), (C 2, 2), (C 3, 2), (C 4, 2),
                                                                 (C 0, 3), (C 1, 3), (C 2, 3), (C 3, 3), (C 4, 3),
                                                                 (C 0, 4), (C 1, 4), (C 2, 4), (C 3, 4), (C 4, 4)])

  describe "adjacent" $ do
    it "should return True for a point adjacent to a segment" $ do
      property $ forAll (genCoord) $ \coord ->
        adjacent coord (HSegement (C 0 0) 5) ==> (coord `elem` [(C -1, 0), (C 5, 0), (C 0, -1), (C 0, 1)])

    it "should return True for a point adjacent to a rectangle" $ do
      property $ forAll (genCoord) $ \coord ->
        adjacent coord (Rectangle (C 0 0) 5 5) ==> (coord `elem` [(C -1, 0), (C 5, 0), (C 0, -1), (C 0, 5),
                                                                 (C -1, 1), (C -1, 2), (C -1, 3), (C -1, 4),
                                                                 (C 5, 1), (C 5, 2), (C 5, 3), (C 5, 4),
                                                                 (C 1, -1), (C 2, -1), (C 3, -1), (C 4, -1),
                                                                 (C 1, 5), (C 2, 5), (C 3, 5), (C 4, 5)])

  describe "collision" $ do
    it "should return True for two overlapping rectangles" $ do
      property $ forAll (genRectangle) $ \r1 ->
        forAll (genRectangle) $ \r2 ->
          collision r1 r2 ==> (collision_approx r1 r2)

    it "should return False for two non-overlapping rectangles" $ do
      property $ forAll (genRectangle) $ \r1 ->
        forAll (genRectangle) $ \r2 ->
          not (collision r1 r2) ==> (not (collision_approx r1 r2))

  describe "adjacentes" $ do
    it "should return True for two adjacent rectangles" $ do
      property $ forAll (genRectangle) $ \r1 ->
        forAll (genRectangle) $ \r2 ->
          adjacentes r1 r2 ==> (collision_approx r1 r2)

    it "should return False for two non-adjacent rectangles" $ do
      property $ forAll (genRectangle) $ \r1 ->
        forAll (genRectangle) $ \r2 ->
          not (adjacentes r1 r2) ==> (not (collision_approx r1 r2))


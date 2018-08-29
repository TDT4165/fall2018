import Test.Hspec
import Test.QuickCheck

import Lib as L

genNeg :: Gen Int
genNeg = choose (-25, -1)

genPos :: Gen Int
genPos = choose (0, 12000)

negList = [-10..(-1)]
mixList = [-10, -4, 347, -9, 0, 0, 2, -2, 0, 1, -27]
manyCheck = [[], negList, mixList]

main :: IO ()
main = hspec $ do

    describe "f0" $ do
        it "returns a String for a String input" $ do
            L.f0 "a string" `shouldBe` Prelude.id "a string"
        it "returns a Bool for a Bool input" $ do
            L.f0 True `shouldBe` Prelude.id True
        it "returns a Double for a Double input" $ do
            L.f0 (3.0 :: Double) `shouldBe` Prelude.id (3.0 :: Double)

    describe "f1" $ do
        it "returns a String for a String -> Int input" $ do
            L.f1 "monkey" 5 `shouldBe` "monkey"
        it "returns a Bool for a Bool -> String input" $ do
            L.f1 True "monkey" `shouldBe` True

    describe "f2" $ do        
        it "returns an Int for a String -> Int input" $ do
            L.f2 "monkey" 5 `shouldBe` 5
        it "returns a String for a Bool -> String input" $ do
            L.f2 True "monkey" `shouldBe` "monkey"

    describe "take" $ do
        it "returns an empty list for negative integers" $ do
            property $ forAll genNeg $ \n -> L.take n [1..10] == ([]:: [Int])
        it "returns an empty list for n = 0" $ do
            L.take 0 ['a'..'z'] `shouldBe` ([] :: [Char])
        it "returns an empty list for []" $ do
            L.take 5 [] `shouldBe` ([] :: [Int])
        it "returns the n first elements of a list" $ do
            L.take 5 ['a'..'z'] `shouldBe` ['a'..'e']
        it "returns the whole list if it's shorter than n" $ do
            L.take 10 [1..5] `shouldBe` [1..5]

    describe "safeHeadList" $ do
      it "returns empty list for empty list" $ do
        L.safeHeadList ([] :: [Int]) `shouldBe` ([] :: [Int])
      it "returns singleton consisting of first element of non-empty list" $ do
        L.safeHeadList [1..10] `shouldBe` [1]

    describe "safeHead" $ do
      it "returns None for empty list" $ do
        L.safeHead [] `shouldBe` (L.None :: L.Maybe Int)
      it "returns Some of head of non-empty list" $ do
        L.safeHead [1..10] `shouldBe` L.Some 1

    describe "map" $ do
        it "returns an empty list for f []" $ do
            L.map (+1) [] `shouldBe` []
        it "applies a function to all elements" $ do
            L.map (+2) [1..10] `shouldBe` [3..12]

    describe "iterate" $ do
        it "creates a list of multiples of 5" $ do
            property $ forAll genPos $ \n -> (L.iterate (+5) 0) !! n == n*5
        it "creates a list where each element prepends space to a string" $ do
            property $ forAll genPos $ \n -> (L.iterate (' ':) "") !! n == replicate n ' '

    describe "filterPos" $ do
        it "returns empty list for empty list" $ do
            filterPos [] `shouldBe` []
        it "returns empty list for list of negative integers" $ do
            filterPos negList `shouldBe` []
        it "filters out negative entries of a list" $ do
            filterPos mixList `shouldBe` [347, 0, 0, 2, 0, 1]

    describe "filterPosMany" $ do
        it "filters all lists in a list" $ do
            filterPosMany manyCheck `shouldBe` [[], [], [347, 0, 0, 2, 0, 1]]

    describe "flip3" $ do
        it "reverses the three arguments" $ do
            flip3 (\x y z -> [x,y,z]) 'c' 'b' 'a' `shouldBe` "abc"

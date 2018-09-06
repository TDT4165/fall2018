import Test.Hspec
import Test.QuickCheck

import qualified Lib as L
import Tree

genIntLst :: Gen [Integer]
genIntLst = listOf1 $ choose (0,300)

genStrLst :: Gen [String]
genStrLst = listOf1 $ sublistOf ['.'..'z']

data Color = Red | Green | Blue | Yellow deriving (Show, Eq, Ord)

tree = Branch (Branch (Leaf 2) 7 (Leaf 5)) 4 (Leaf 9) 

main :: IO ()
main = hspec $ do
    describe "listSum" $ do
        it "sums a list of integers" $ do
            property $ forAll genIntLst $ \n -> L.listSum n == sum n
        it "returns 0 for empty list" $ do
            L.listSum [] `shouldBe` 0

    describe "listProduct" $ do
        it "calculates the product of a list of doubles" $ do
            property $ forAll genIntLst $ \n -> L.listProduct n == product n
        it "returns 1 for empty list" $ do
            L.listProduct [] `shouldBe` 1

    describe "listConcat" $ do
        it "concats a list of strings" $ do
            property $ forAll genStrLst $ \n -> L.listConcat n == concat n
        it "returns [] for empty list" $ do
            L.listConcat [] `shouldBe` ([] :: [Int])

    describe "listMaximum" $ do
        it "finds the largest element in a list of integers" $ do
            property $ forAll genIntLst $ \n -> (==) (L.listMaximum n) (Just $ maximum n)
        it "finds the largest element in a list of strings" $ do
            property $ forAll genStrLst $ \n -> (==) (L.listMaximum n) (Just $ maximum n)
        it "returns Nothing for empty list" $ do
            L.listMaximum [] `shouldBe` (Nothing :: Maybe Int)
    
    describe "listMinimum" $ do
        it "finds the smallest element in a list of integers" $ do
            property $ forAll genIntLst $ \n -> (==) (L.listMinimum n) (Just $ minimum n)
        it "finds the smallest element in a list of strings" $ do
            property $ forAll genStrLst $ \n -> (==) (L.listMinimum n) (Just $ minimum n)
        it "returns Nothing for empty list" $ do
            L.listMinimum [] `shouldBe` (Nothing :: Maybe Int)
    
    describe "sum" $ do
        it "sums a list of integers" $ do
            property $ forAll genIntLst $ \n -> L.sum n == sum n
        it "returns 0 for empty list" $ do
            L.sum [] `shouldBe` 0
 
    describe "concat" $ do
        it "concats a list of strings" $ do
            property $ forAll genStrLst $ \n -> L.concat n == concat n
        it "returns [] for empty list" $ do
            L.concat [] `shouldBe` ([] :: [Int])
    
    describe "length" $ do
        it "calculates the length of a list" $ do
            property $ forAll genIntLst $ \n -> L.length n == length n
        it "returns 0 for empty list" $ do
            L.length [] `shouldBe` 0

    describe "elem" $ do
        it "finds an element in a list" $ do
            L.elem 'a' ['.'..'z'] `shouldBe` True
        it "returns False if element is not in list" $ do
            L.elem 437 [1..300] `shouldBe` False
        it "returns False for empty list" $ do
            L.elem "snowden" [] `shouldBe` False

    describe "safeMaximum" $ do
        it "finds the largest element in a list of integers" $ do
            property $ forAll genIntLst $ \n -> (==) (L.safeMaximum n) (Just $ maximum n)
        it "finds the largest element in a list of strings" $ do
            property $ forAll genStrLst $ \n -> (==) (L.safeMaximum n) (Just $ maximum n)
        it "returns Nothing for empty list" $ do
            L.safeMaximum [] `shouldBe` (Nothing :: Maybe Int)
    
    describe "safeMinimum" $ do
        it "finds the smallest element in a list of integers" $ do
            property $ forAll genIntLst $ \n -> (==) (L.safeMinimum n) (Just $ minimum n)
        it "finds the smallest element in a list of strings" $ do
            property $ forAll genStrLst $ \n -> (==) (L.safeMinimum n) (Just $ minimum n)
        it "returns Nothing for empty list" $ do
            L.safeMinimum [] `shouldBe` (Nothing :: Maybe Int)

    describe "any" $ do
        it "returns True if an element satisfies the predicate" $ do
            L.any (>Green) [Blue, Green, Yellow] `shouldBe` True
        it "returns False if no elements satisfies the predicate" $ do
            L.any (==Green) [Blue, Blue, Blue] `shouldBe` False
        it "returns False for empty list" $ do
            L.any (==True) [] `shouldBe` False

    describe "all" $ do
        it "returns True if all elements satisfy the predicate" $ do
            L.all (==Red) [Red, Red, Red] `shouldBe` True
        it "returns False if one element does not satisfy the predicate" $ do
            L.all (==Green) [Green, Green, Blue] `shouldBe` False
        it "returns True for empty list" $ do
            L.all (==True) [] `shouldBe` True

    describe "Num Complex is implemented" $ do
        it "adds, multiplies and subtracts Complex numbers" $ do
            (L.Complex 3 4) - (L.Complex 2 7) + (L.Complex 7 5) * (L.Complex 1 2)
              `shouldBe` (L.Complex (-2) 16)
        it "computes the sign of a Complex number" $ do
            signum (L.Complex 3 4) `shouldBe` (L.Complex 0.6 0.8)
        it "computes the absolute value of a Complex number" $ do
            abs (L.Complex 3 4) `shouldBe` (L.Complex 5 0)
        it "converts integers to complex numbers" $ do
            fromInteger 27 `shouldBe` (L.Complex 27 0)

    describe "foldable instance of Tree" $ do
        it "can be summed" $ do
            sum tree `shouldBe` (27 :: Int)

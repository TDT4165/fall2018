import Test.Hspec
import Test.QuickCheck

import Lib

genPos :: Gen Int
genPos = choose (1, 10000)

genNeg :: Gen Int
genNeg = choose (-25, -1)

fb = ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz","13","14","FizzBuzz","16","17","Fizz","19","Buzz","Fizz","22","23","Fizz","Buzz","26","Fizz","28","29","FizzBuzz","31","32","Fizz","34","Buzz","Fizz","37","38","Fizz","Buzz","41","Fizz","43","44","FizzBuzz","46","47","Fizz","49","Buzz","Fizz","52","53","Fizz","Buzz","56","Fizz","58","59","FizzBuzz","61","62","Fizz","64","Buzz","Fizz","67","68","Fizz","Buzz","71","Fizz","73","74","FizzBuzz","76","77","Fizz","79","Buzz","Fizz","82","83","Fizz","Buzz","86","Fizz","88","89","FizzBuzz","91","92","Fizz","94","Buzz","Fizz","97","98","Fizz","Buzz"]

fib21 = [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]

cart = [(4,3),(4,7),(4,9),(6,3),(6,7),(6,9),(8,3),(8,7),(8,9)]

vowels = "aeiouAEIOU"
nonVowels = "0123456789:;<=>?@BCDFGHJKLMNPQRSTVWXYZ[\\]^_`bcdfghjklmnpqrstvwxyz"

main :: IO ()
main = hspec $ do

    describe "add" $ do
        it "can compute 2 + 2 = 4" $ do
            add 2 2 `shouldBe` 4

        it "has zero as a left identity" $ do
            property $ \n -> add 0 n == n

        it "has zero as a right identity" $ do
            property $ \n -> add n 0 == n

        it "is commutative" $ do
            property $ \m n -> add m n == add n m

        it "is associative" $ do
            property $ \m n o -> add m (add n o) == add (add m n) o

    describe "isVowel" $ do
        it "returns True for all vowels" $ do
            map isVowel vowels `shouldBe` replicate (length vowels) True
        it "returns False for other characters" $ do
            map isVowel nonVowels `shouldBe` replicate (length nonVowels) False

    describe "amountOf" $ do
        it "maps to correct tuples" $ do
            map (amountOf "Haskell") [[], [1], [1,2], [1,2,3]] `shouldBe` zip [None, One, Two, Three] (replicate 4 "Haskell")

    describe "fib" $ do
        it "can compute the first 21 fibonnaci numbers" $ do
            map fib [0..20] `shouldBe` fib21
    
    describe "ending" $ do
        it "adds -ing to a list of words" $ do
            ending ["jump", "eat", "try"] `shouldBe` ["jumping", "eating", "trying"]
        it "removes empty strings" $ do
            ending ["", "sing", "", ""] `shouldBe` ["singing"]

    describe "takeInt" $ do
        it "returns an empty list for negative integers" $ do
            property $ forAll genNeg $ \n -> takeInt n [1..10] == []
        it "returns an empty list for n = 0" $ do
            takeInt 0 [1..10] `shouldBe` []
        it "returns an empty list for []" $ do
            takeInt 5 [] `shouldBe` ([] :: [Int])
        it "returns the n first elements of a list" $ do
            takeInt 5 [1..10] `shouldBe` [1..5]
        it "returns the whole list if it's shorter than n" $ do
            takeInt 10 [1..5] `shouldBe` [1..5]

    describe "fizzbuzz" $ do
        it "fizzbuzz is correct" $ do
            fizzbuzz `shouldBe` fb

    describe "listOfEven" $ do
        it "is an infinite list of even numbers" $ do
            property $ forAll genPos $ \n -> listOfEven !! n == toInteger (2*n)

    describe "zipped" $ do
        it "equals the zipped list of [1..26] and ['a'..'z']" $ do
            zipped `shouldBe` zip [1..26] ['a'..'z']

    describe "cartesian" $ do
        it "equals the cartesian product of [4,6,8] and [3,7,9]" $ do
            cartesian `shouldBe` cart

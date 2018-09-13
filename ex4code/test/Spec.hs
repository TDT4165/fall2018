import Test.Hspec
import Test.QuickCheck

import Lib
import Prelude hiding (lex)

genList :: Gen [Char]
genList = sublistOf $ '.':[':'..'~']

recList = [1,2,3,4,5] ++ recList

main :: IO ()
main = hspec $ do
    describe "takeWhile" $ do
      it "correctly takes positive elements" $ do
        Lib.takeWhile (>0) [3,2 .. -3] `shouldBe` [3,2,1]
      it "correctly handles empty-list" $ do
        Lib.takeWhile (==0) [] `shouldBe` ([] :: [Int])

    describe "dropWhile" $ do
      it "correctly drops positive elements" $ do
        Lib.dropWhile (>0) [3,2 .. -3] `shouldBe` [0,-1 .. -3]
      it "correctly handles empty-list" $ do
        Lib.dropWhile (==0) [] `shouldBe` ([] :: [Int])

    describe "break" $ do
      it "correctly breaks on zero" $ do
        Lib.break (==0) [-2..2] `shouldBe` ([-2,-1], [0..2])
      it "correctly handles empty-list" $ do
        Lib.break (==0) [] `shouldBe` ([] :: [Int], [] :: [Int])

    describe "splitOn" $ do
        it "splits a list of integers on a given integer" $ do
            take 3 (splitOn 1 recList) `shouldBe` replicate 3 [2,3,4,5]
        it "returns an empty list for empty list" $ do
            splitOn 'a' [] `shouldBe` ([] :: [String])
        it "removes excess elements" $ do
            splitOn ';' ";;;;this;;is;;;;;sparta;;;" `shouldBe` ["this", "is", "sparta"]

    describe "lex" $ do
        it "splits a string on space" $ do
            lex "  we need    bees  " `shouldBe` ["we", "need", "bees"]

    describe "tokenize" $ do
        it "tokenizes operators" $ do
            tokenize ["*", "+", "-", "/"] `shouldBe` [TokOp Mult, TokOp Plus, TokOp Minus, TokOp Div]
        it "tokenizes integers" $ do
            tokenize ["1", "23", "3754924"] `shouldBe` [TokInt 1, TokInt 23, TokInt 3754924]
        it "recognizes erroneous strings" $ do
            property $ forAll genList $ \n -> tokenize [n] == [TokErr]
        it "returns empty list for empty input" $ do
            tokenize [] `shouldBe` ([] :: [Token])

    describe "interpret" $ do
        it "interprets a list of Tokens and returns the result" $ do
            interpret [TokInt 230, TokInt 5, TokOp Plus, TokInt 10, TokOp Minus] `shouldBe` [TokInt 225]
        it "returns empty list for empty input" $ do
            interpret [] `shouldBe` ([] :: [Token])
        it "returns error token for erroneous input" $ do
            interpret [TokErr] `shouldBe` [TokErr]

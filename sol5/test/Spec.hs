import Test.Hspec
import Test.QuickCheck

import Lib
import Prelude hiding (lex)

genList :: Gen [Char]
genList = sublistOf $ '.':[':'..'~']

recList = [1,2,3,4,5] ++ recList

main :: IO ()
main = hspec $ do
    describe "tokenize" $ do
        it "tokenizes operators" $ do
            tokenize ["*", "+", "-", "/"] `shouldBe` Just [TokOp Mult, TokOp Plus, TokOp Minus, TokOp Div]
        it "tokenizes integers" $ do
            tokenize ["1", "23", "3754924"] `shouldBe` Just [TokInt 1, TokInt 23, TokInt 3754924]
        it "recognizes erroneous strings" $ do
            property $ forAll genList $ \n -> tokenize [n] == Nothing
        it "returns empty list for empty input" $ do
            tokenize [] `shouldBe` (Just [] :: Maybe [Token])

    describe "interpret" $ do
        it "interprets a list of Tokens and returns the result" $ do
            interpret (Just [TokInt 230, TokInt 5, TokOp Plus, TokInt 10, TokOp Minus]) `shouldBe` Just [TokInt 225]
        it "returns empty list for empty input" $ do
            interpret (Just []) `shouldBe` (Just [] :: Maybe [Token])
        it "returns error token for erroneous input" $ do
            interpret Nothing `shouldBe` Nothing

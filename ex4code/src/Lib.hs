module Lib
    ( Token(..)
    , Op(..)
    , takeWhile
    , dropWhile
    , break
    , splitOn
    , lex
    , tokenize
    , interpret
    , shunt
    ) where

import Prelude hiding (lex, dropWhile, takeWhile, break)
import Data.Char (isDigit)

takeWhile, dropWhile :: (a -> Bool) -> [a] -> [a]

takeWhile = undefined

dropWhile = undefined

break :: (a -> Bool) -> [a] -> ([a], [a])
break = undefined

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = undefined

data Token = TokOp Op
           | TokInt Int
           | TokErr
           deriving (Eq, Show)

data Op = Plus
        | Minus
        | Div
        | Mult
        deriving (Show, Eq)

lex :: String -> [String]
lex = undefined

tokenize :: [String] -> [Token]
tokenize = undefined

interpret :: [Token] -> [Token]
interpret = undefined

opLeq :: Token -> Token -> Bool
opLeq = undefined

shunt :: [Token] -> [Token]
shunt = undefined

shuntInternal :: [Token] -> [Token] -> [Token] -> [Token]
shuntInternal = undefined

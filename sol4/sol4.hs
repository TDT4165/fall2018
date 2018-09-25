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
takeWhile _ [] = []
takeWhile p (x:xs)
  | p x       = x : takeWhile p xs
  | otherwise = []

dropWhile _ [] = []
dropWhile p l@(x:xs)
  | p x       = dropWhile p xs
  | otherwise = l

break :: (a -> Bool) -> [a] -> ([a], [a])
break p l = (start, end)
  where start = takeWhile (not . p) l
        end = drop (length start) l

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn ch lst = let strip = dropWhile (==ch) lst
                 in case strip of
                    []     -> []
                    (x:xs) -> n : (splitOn ch b)
                      where (n, b) = break (==ch) strip

data Token = TokOp Op
           | TokInt Int
           | TokErr
           deriving (Eq, Show)

data Op = Plus
        | Minus
        | Div
        | Mult
        | Dupl
        | Flip
        deriving (Show, Eq)

isInt :: String -> Bool
isInt str = all isDigit str

lex :: String -> [String]
lex lst = splitOn ' ' lst

token :: String -> Token
token "+"  = TokOp Plus
token "-"  = TokOp Minus
token "/"  = TokOp Div
token "*"  = TokOp Mult
token "#"  = TokOp Dupl
token "--" = TokOp Flip
token lst
    | isInt lst = TokInt (read lst)
    | otherwise = TokErr

tokenize :: [String] -> [Token]
tokenize lst = let tokens = map token lst
                in case filter (== TokErr) tokens of
                    [] -> tokens
                    _  -> [TokErr]            

calc :: [Token] -> Token -> [Token]
calc (TokInt x:TokInt y:xs) (TokOp Plus)  = TokInt (x + y):xs
calc (TokInt x:TokInt y:xs) (TokOp Minus) = TokInt (y - x):xs
calc (TokInt x:TokInt y:xs) (TokOp Mult)  = TokInt (x * y):xs
calc (TokInt x:TokInt y:xs) (TokOp Div)   = TokInt (y `div` x):xs
calc (tok:xs) (TokOp Dupl)                = tok:tok:xs
calc (TokInt x:xs) (TokOp Flip)           = TokInt (negate x):xs
calc lst tok                              = tok:lst

interpret :: [Token] -> [Token]
interpret []       = []
interpret [TokErr] = [TokErr]
interpret lst      = foldl calc [] lst

prec :: Token -> Int
prec (TokOp Flip)  = 3
prec (TokOp Dupl)  = 3
prec (TokOp Mult)  = 2
prec (TokOp Div)   = 2
prec (TokOp Plus)  = 1
prec (TokOp Minus) = 1

opLeq :: Token -> Token -> Bool
opLeq x y = prec x <= prec y

shunt :: [Token] -> [Token]
shunt [TokErr] = [TokErr]
shunt lst      = si [] [] lst

si :: [Token] -> [Token] -> [Token] -> [Token]
si ops out []                = reverse (ops ++ out)
si ops out (op@(TokOp _):xs) = opCompare op ops out xs
si ops out (num:xs)          = si ops (num:out) xs

opCompare :: Token -> [Token] -> [Token] -> [Token] -> [Token]
opCompare x (y:xs) out inp
  | opLeq x y = opCompare x xs (y:out) inp
  | otherwise = si (x:y:xs) out inp
opCompare x ops out inp    = si (x:ops) out inp

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

import Prelude hiding (lex)
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Control.Monad (foldM)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn ch lst = let strip = dropWhile (==ch) lst
                 in case strip of
                    []     -> []
                    (x:xs) -> n : (splitOn ch b)
                                where
                              (n, b) = break (==ch) strip

data Token = TokOp Op
           | TokInt Int
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

opToTokOp :: String -> Maybe Op
opToTokOp "+"  = Just Plus
opToTokOp "-"  = Just Minus
opToTokOp "/"  = Just Div
opToTokOp "*"  = Just Mult
opToTokOp "#"  = Just Dupl
opToTokOp "--" = Just Flip
opToTokOp _    = Nothing

token :: String -> Maybe Token
token lst
    | isInt lst = Just $ TokInt (read lst)
    | otherwise = case opToTokOp lst of
                      Just x -> Just (TokOp x)
                      _      -> Nothing

tokenize :: [String] -> Maybe [Token]
tokenize lst = anyNothing $ map token lst

anyNothing :: Eq a => [Maybe a] -> Maybe [a]
anyNothing lst = case filter (==Nothing) lst of
                  [] -> Just (map fromJust lst)
                  _  -> Nothing

calc :: [Token] -> Token -> Maybe [Token]
calc (TokInt x:TokInt y:xs) (TokOp Plus)  = Just $ TokInt (x + y):xs
calc (TokInt x:TokInt y:xs) (TokOp Minus) = Just $ TokInt (y - x):xs
calc (TokInt x:TokInt y:xs) (TokOp Mult)  = Just $ TokInt (x * y):xs
calc (TokInt x:TokInt y:xs) (TokOp Div)   = case x of
                                             0 -> Nothing
                                             _ -> Just $ TokInt (y `div` x):xs
calc (tok:xs) (TokOp Dupl)                = Just $ tok:tok:xs
calc (TokInt x:xs) (TokOp Flip)           = Just $ TokInt (negate x):xs
calc lst tok                              = Just $ tok:lst

interpret :: Maybe [Token] -> Maybe [Token]
interpret Nothing    = Nothing
interpret (Just lst) = foldM calc [] lst

prec :: Token -> Int
prec (TokOp Flip)  = 3
prec (TokOp Dupl)  = 3
prec (TokOp Mult)  = 2
prec (TokOp Div)   = 2
prec (TokOp Plus)  = 1
prec (TokOp Minus) = 1

opLeq :: Token -> Token -> Bool
opLeq x y = prec x <= prec y

shunt :: Maybe [Token] -> Maybe [Token]
shunt Nothing    = Nothing
shunt (Just lst) = Just $ si [] [] lst

si :: [Token] -> [Token] -> [Token] -> [Token]
si ops out []                = reverse (ops ++ out)
si ops out (op@(TokOp _):xs) = opCompare op ops out xs
si ops out (num:xs)          = si ops (num:out) xs

opCompare :: Token -> [Token] -> [Token] -> [Token] -> [Token]
opCompare x (y:xs) out inp
    | opLeq x y = opCompare x xs (y:out) inp
    | otherwise = si (x:y:xs) out inp
opCompare x ops out inp    = si (x:ops) out inp

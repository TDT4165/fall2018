module Main where

import Lib
import Control.Monad
import System.Environment

parseInfix :: String -> [Token]
parseInfix = interpret . shunt . tokenize . Lib.lex

main :: IO ()
main = forever $ getLine >>= print . parseInfix

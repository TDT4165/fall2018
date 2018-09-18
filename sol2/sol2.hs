module Lib
    ( f0
    , f1
    , f2
    , take
    , map
    , iterate
    , filterPos
    , filterPosMany
    , Maybe(..)
    , safeHeadList
    , safeHead
    ) where

import Prelude hiding (map, take, iterate, sqrt, neg, succ, pred, Maybe)

-- TASK 1
-- Parametric polymorphism

f0 :: a -> a
f0 x = x

f1 :: a -> b -> a
f1 a _ = a

f2 :: a -> b -> b
f2 _ b = b

-- An answer that the types of f0, f1 and f2 have 
-- one inhabitant is accepted.
-- Since
f3 :: a -> a
f3 = error "another inhabitant"
-- also type checks, this is also an accepted answer
-- error is equivalent to undefined and inhabits every type

-- If we fix a = Int, we have more inhabitants to work with, e.g.
neg :: Int -> Int
neg x = -x

succ :: Int -> Int
succ x = x+1

pred :: Int -> Int
pred x = x-1

--same as ex1, just change [Int] to [a]
take :: Int -> [a] -> [a]
take _ [] = []
take n (x:xs)
    | n <= 0    = []
    | otherwise = x : take (n-1) xs

data Maybe a = Some a | None deriving (Eq, Show)

safeHeadList = take 1

safeHead [] = None
safeHead (x:_) = Some x

-- TASK 2
-- Higher order functions

map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

-- TASK 3
-- Currying and partial application

filterPos :: [Int] -> [Int]
filterPos lst = filter (>=0) lst

filterPosMany :: [[Int]] -> [[Int]]
filterPosMany lst = map filterPos lst

flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 f c b a = f a b c

-- TASK 4
-- Infinite lists

newtons :: Double -> Double -> Double
newtons x guess = guess - (guess^2 - x)/(2*guess)

ourInfLst :: Double -> Double -> [Double]
ourInfLst x guess = iterate (newtons x) guess

doubleIsInt :: Double -> Bool
doubleIsInt x = fromInteger (round x) == x

approx :: Double -> [Double] -> Double
approx diff (x:y:xs)
    | abs (x-y) <= diff = y
    | otherwise         = approx diff (y:xs)

isPerfSq :: Double -> Bool
isPerfSq x = doubleIsInt . approx 0.00000001 $ ourInfLst x (x/2)

--tested up to 2000
accuracy :: Int -> Bool
accuracy x = take x generated == take x [x^2 | x <- [1..]]
                where
             zpd       = zip [1..] (map isPerfSq [1..])
             f (x,y)   = y == True
             generated = fst . unzip $ filter f zpd

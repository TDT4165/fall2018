module Lib
    ( listSum
    , listProduct
    , listConcat
    , listMaximum
    , listMinimum
    , sum
    , concat
    , length
    , elem
    , safeMaximum
    , safeMinimum
    , any
    , all
    , foldr
    , Complex(..)
    ) where

import Prelude hiding (foldr, maximum, minimum, any, all, length
                      , concat, sum, product, elem, Foldable(..))

-- TASK 2
-- Bounded parametric polymorphism

-- Implement the following functions that reduce a list to a single
-- value (or Maybe a single value).

-- Maybe is imported from Prelude and is defined like this:
-- data Maybe a = Just a | Nothing

listSum :: (Num a) => [a] -> a
listSum = undefined

listProduct :: (Num a) => [a] -> a
listProduct = undefined

listConcat :: [[a]] -> [a]
listConcat = undefined

listMaximum :: (Ord a) => [a] -> Maybe a
listMaximum = undefined

listMinimum :: (Ord a) => [a] -> Maybe a
listMinimum = undefined

-- TASK 3 Folds

-- TASK 3.1
-- Below our Foldable class is defined. Now define a list instance of
-- Foldable, and then define the Foldable versions of the functions
-- you defined previously (and some more).
class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b

instance Foldable [] where
  foldr = undefined

--
-- USE FOLDR TO DEFINE THESE FUNCTIONS
--
sum :: (Num a, Foldable t) => t a -> a
sum = undefined

concat :: Foldable t => t [a] -> [a]
concat = undefined

length :: Foldable t => t a -> Int
length = undefined

elem :: (Eq a, Foldable t) => a -> t a -> Bool
elem = undefined

safeMaximum :: (Foldable t, Ord a) => t a -> Maybe a
safeMaximum = undefined

safeMinimum :: (Foldable t, Ord a) => t a -> Maybe a
safeMinimum = undefined

-- The functions "any" and "all" check if any or all elements of a
-- Foldable satisfy the given predicate.
--
-- USE FOLDR
--
any :: Foldable t => (a -> Bool) -> t a -> Bool
any p = undefined

all :: Foldable t => (a -> Bool) -> t a -> Bool
all p = undefined

-- TASK 4
-- Num Complex
 
data Complex = Complex Double Double deriving (Eq) 
 
instance Show Complex where 
    show (Complex r i) 
        | i >= 0 = show r ++ "+" ++ show i ++ "i" 
        | otherwise = show r ++ "-" ++ show (abs i) ++ "i" 

instance Num Complex where 
    (+) = undefined
    (*) = undefined
    abs = undefined 
    signum = undefined
    fromInteger = undefined 
    negate = undefined 

-- TASK 5
-- Making your own type classes

type Position = (Double, Double)

class Pos a where
    pos :: a -> Position

data Campus = Kalvskinnet
            | Gløshaugen
            | Tyholt
            | Moholt
            | Dragvoll
            deriving (Show, Eq)

instance Pos Campus where
    pos Kalvskinnet = (63.429, 10.388)
    pos Gløshaugen  = (63.416, 10.403)
    pos Tyholt      = (63.423, 10.435)
    pos Moholt      = (63.413, 10.434)
    pos Dragvoll    = (63.409, 10.471)

--class (Pos a) => Move a where

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
listSum :: (Num a) => [a] -> a
listSum [] = 0
listSum (x:xs) = x + listSum xs

listProduct :: (Num a) => [a] -> a
listProduct [] = 1
listProduct (x:xs) = x * listProduct xs

listConcat :: [[a]] -> [a]
listConcat [] = []
listConcat (x:xs) = x ++ listConcat xs

listMaximum :: (Ord a) => [a] -> Maybe a
listMaximum [] = Nothing
listMaximum (x:xs) = Just $ listMaximum' x xs
  where listMaximum' current [] = current
        listMaximum' current (x:xs)
          | current <= x = listMaximum' x xs
          | otherwise    = listMaximum' current xs

listMinimum :: (Ord a) => [a] -> Maybe a
listMinimum [] = Nothing
listMinimum (x:xs) = Just $ listMinimum' x xs
  where listMinimum' current [] = current
        listMinimum' current (x:xs)
          | current >= x = listMinimum' x xs
          | otherwise    = listMinimum' current xs

-- TASK 3 Folds

-- TASK 3.1
-- Below our Foldable class is defined. Now define a list instance of
-- Foldable, and then define the Foldable versions of the functions
-- you defined previously (and some more).
class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b

instance Foldable [] where
  foldr _ acc [] = acc
  foldr op acc (x:xs) = x `op` foldr op acc xs

--
-- USE FOLDR TO DEFINE THESE FUNCTIONS
--
sum :: (Num a, Foldable t) => t a -> a
sum = foldr (+) 0

concat :: Foldable t => t [a] -> [a]
concat = foldr (++) []

length :: Foldable t => t a -> Int
length = foldr (\_ acc -> acc + 1) 0 

elem :: (Eq a, Foldable t) => a -> t a -> Bool
x `elem ` ys = foldr (\y b -> b || y == x) False ys

safeMaximum :: (Foldable t, Ord a) => t a -> Maybe a
safeMaximum = foldr max' Nothing
  where max' x Nothing = Just x
        max' x jy@(Just y)
          | x >= y     = Just x
          | otherwise  = jy

safeMinimum :: (Foldable t, Ord a) => t a -> Maybe a
safeMinimum = foldr min' Nothing
  where min' x Nothing = Just x
        min' x jy@(Just y)
          | x <= y     = Just x
          | otherwise  = jy

-- The functions "any" and "all" check if any or all elements of a
-- Foldable satisfy the given predicate.
--
-- USE FOLDR
--
any :: Foldable t => (a -> Bool) -> t a -> Bool
any p = foldr (\x y -> p x || y) False

all :: Foldable t => (a -> Bool) -> t a -> Bool
all p = foldr (\x y -> p x && y) True

-- TASK 4
-- Num Complex
 
data Complex = Complex Double Double deriving (Eq) 
 
instance Show Complex where 
    show (Complex r i) 
        | i >= 0 = show r ++ "+" ++ show i ++ "i" 
        | otherwise = show r ++ "-" ++ show (abs i) ++ "i" 

instance Num Complex where 
    (+) (Complex r1 i1) (Complex r2 i2) = Complex (r1+r2) (i1+i2) 
    (*) (Complex r1 i1) (Complex r2 i2) = Complex (r1*r2 - i1*i2) (r1*i2 + i1*r2) 
    abs (Complex r i) = Complex (sqrt (r**2+i**2)) 0 
    signum (Complex r i) = Complex (r/a) (i/a)
                                where
                                a = sqrt (r**2+i**2)
    fromInteger int = Complex (fromInteger int) 0 
    (-) (Complex r1 i1) (Complex r2 i2) = Complex (r1-r2) (i1-i2) 

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

class (Pos a) => Move a where
    relocate :: a -> Position -> a
    belongs :: a -> Position

data Car = Car { brand :: String
               , regnr :: String
               , isAt :: Position
               , key :: Key
               , parking :: Position} deriving (Show)

instance Eq Car where
    (==) car1 car2 = regnr car1 == regnr car2

instance Pos Car where
    pos = isAt

instance Move Car where
    relocate car loc = car { isAt = loc }
    belongs = parking

data Key = Key { keynr :: Int
               , located :: Position
               , cabinet :: Position } deriving (Show)

instance Eq Key where
    (==) key1 key2 = keynr key1 == keynr key2

instance Pos Key where
    pos = located

instance Move Key where
    relocate key loc = key { located = loc }
    belongs = cabinet

free :: Move a => a -> Bool
free object = pos object == belongs object

carAvailable :: Car -> Bool
carAvailable car = free car || (free $ key car)

distBetween :: Pos a => a -> a -> Position
distBetween loc object = (abs $ loc1-obj1 , abs $ loc2-obj2)
                            where
                            (loc1, loc2) = pos loc
                            (obj1, obj2) = pos object

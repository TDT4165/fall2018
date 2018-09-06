module Tree
    (Tree(..)
    ) where

-- TASK 3.2
-- Binary Trees

data Tree a = Branch (Tree a) a (Tree a) | Leaf a
  deriving (Eq, Show)

-- The Foldable instance might prove tricky to define, so
-- defining the specific functions first may be easier!
treeSum :: (Num a) => Tree a -> a
treeSum = undefined

treeConcat :: Tree String -> String
treeConcat = undefined

treeMaximum :: (Ord a) => Tree a -> a
treeMaximum = undefined

-- Write a Foldable instance for Tree.
instance Foldable Tree where
    foldr = undefined

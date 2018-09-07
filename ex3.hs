listPattern :: Show a => [a] -> String
listPattern []         = "This matches an empty list"
listPattern lst@(x:xs) = "x: " ++ show x ++ "\nxs: " ++
                         show xs ++ "\nlst: " ++ show lst

maybePattern whole@(Just a) = "whole: " ++ show whole ++
                              "\na: " ++ show a

--class Show a where
--    show :: a -> String

data Person = Person { firstName :: String
                     , lastName :: String
                     , socialSecNr :: Int
                     , zipCode :: Int
                     }

person1 = Person { firstName = "Jane"
                 , lastName = "Doe"
                 , socialSecNr = 1234567890
                 , zipCode = 7034}

instance Show Person where
    show pers = firstName pers ++ " " ++ lastName pers
                ++ ", " ++ (show . socialSecNr $ pers)

instance Eq Person where
    pers1 == pers2 = socialSecNr pers1 == socialSecNr pers2
 
instance Ord Person where
    pers1 <= pers2 = (lastName pers1 <= lastName pers2)
                     && (firstName pers1 <= firstName pers2)
                     && (socialSecNr pers1 <= socialSecNr)

data Maybe' a = Just' a | Nothing' deriving (Show)

instance Eq a => Eq (Maybe' a) where
    Nothing' == Nothing'   = True
    (Just' x) == (Just' y) = x == y
    _ == _                 = False



instance Ord a => Ord (Maybe' a) where
    (<=) Nothing' _    = True
    (<=) _ Nothing'    = False
    (<=) (Just' a) (Just' b) = a <= b

data Tree a = Branch (Tree a) a (Tree a) | Leaf a deriving (Show, Eq)

example w@(Leaf a) = undefined
example w@(Branch left val right) = undefined


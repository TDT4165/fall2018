import Prelude hiding (toInteger, fromInteger)

data Nat = S Nat | Zero deriving (Show, Eq)

add :: Nat -> Nat -> Nat
add Zero two    = two
add (S n) two = S $ add n two

fromInteger :: Int -> Nat
fromInteger 0 = Zero
fromInteger n = S $ fromInteger (n-1)

toInteger :: Nat -> Int
toInteger Zero = 0
toInteger (S n) = 1 + toInteger n

fib :: Nat -> Nat
fib Zero     = Zero
fib (S Zero) = S Zero
fib n        = add (fib $ minus n $ fromInteger 2) (fib $ minus n $ S Zero)
                    where
                minus two Zero    = two
                minus (S n) (S m) = minus n m

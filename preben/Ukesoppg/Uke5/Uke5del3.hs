module Uke5del3 where
data Expr = S  |  U  |  Og Expr Expr  |  El Expr Expr | Ik Expr deriving Show

--data Expr = Val Int | Add Expr Expr
--folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
--folde f g (Val val) = f val
--folde f g (Add a b) = g (folde f g a) (folde f g b)

-- C a)
folde' :: a -> a -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> Expr -> a
folde' s u o e i (S) = s
folde' s u o e i (U) = u
folde' s u o e i (Og a b) = o (folde' s u o e i a) (folde' s u o e i b)
folde' s u o e i (El a b) = e (folde' s u o e i a) (folde' s u o e i b)
folde' s u o e i (Ik x) = i (folde' s u o e i x )

-- C b)
evb :: Expr -> Bool
evb = folde' True False (&&) (||) not

--C c)
evi :: Expr -> Int
evi = folde' 1 5 (+) (*) negate

--C d)
evh :: Expr -> Int
evh = folde' 1 1 (\c b -> max c b +1) (\c b -> max c b +1) (\a -> 1 + a)









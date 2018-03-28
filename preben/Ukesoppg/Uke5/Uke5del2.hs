module Uke5del2 where


--mult :: Nat -> Nat -> Nat




-- 8.5 Hvis folde kun får en value val returnerer den en type a av den verdien. Hvis den får et uttrykk legger den de sammen med g)
data Expr = Val Int | Add Expr Expr
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val val) = f val
folde f g (Add a b) = g (folde f g a) (folde f g b)

-- 8.6
eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (\x -> 1) (+)
module Oving2 where
data Tre = Leaf Int | Node Tre Int Tre | Emp
list (Leaf y) = [y]
list (Node | y r ) = list | ++ [y] ++ list r
find x (Leaf y) = if x==y then y else error "No " ++ show x
find x (Node | n r ) | x==n = n
             | x<n = find x |
             | otherwise = find x r




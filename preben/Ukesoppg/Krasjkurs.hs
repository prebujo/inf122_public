module Krasjkurs where
import Data.Char
--filter' :: (a -> Bool) -> [a] -> [a]
--filter' _ [] = []
--filter' f (x:xs) = if (f x) then
--            x : (filter' xs)
--            else filter' xs
--myFunc' :: (a -> Bool)
--myFunc' x = even x

--fmyFunc = filter' myFunc'

fst5 :: [a] -> [a]
fst5 = take 5

sumL :: Num a => [a] -> a
sumL = foldr (+) 0

--fib :: Int -> Int
--fib n = n !! (fibonacci n)

--fibonacci :: Int -> [Int]
--fibonacci n = take n (fib' [0,1])

--fib' xs = xs ++ (sum (take 2 (reverse xs )))
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib' (n-1) 1 0
  where
    fib' 0 t1 _ = t1
    fib' n t1 t2 = fib' (n-1) (t1+t2) t1



--0 1 1 2 3 5 8 13

fjernSiff :: [Char] -> [Char]
fjernSiff = filter (\x -> x < '0' && x > '9')

noDigit = not . isDigit

fjernParTall :: [Int] -> [Int]
fjernParTall = filter (\x -> (mod x 2) == 0)

parList = filter even [0..]
parList' = map (*2) [0..]

kvadList = map (^2) [0..]
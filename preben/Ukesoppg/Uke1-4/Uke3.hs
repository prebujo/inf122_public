module Uke3 where
import Data.Char

-- 4.1
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
    where n = length xs `div` 2

-- 4.2
third :: [a] -> a
third xs = head(tail(tail xs))

third2 :: [a] -> a
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (_:_:x:_) = x

-- 4.3
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail1 xs | xs == []   = []
             | otherwise  = tail xs

--safetail2 :: (a,b) -> [a]
--safetail2 (_,xs) = [xs]
--safetail2 _ = []

-- 4.4
--(||) :: Bool -> Bool -> Bool
--True || _ = True
-- True || b = True

-- 4.5
--(&&&) :: Bool -> Bool -> Bool
--(&&&) a b = if a then b else False
--(&&&&) a b = if a then
--           if b then True
--           else False
--           else False

--4.6

-- 4.7
mult :: Int -> (Int -> (Int -> Int))
mult = \x -> (\y -> (\z -> x * y * z))

--4.8/C
luhnDouble :: Int -> Int
luhnDouble x = if y > 9 then y - 9
               else y
               where y = 2 * x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = (z + luhnDouble y + x + luhnDouble w) `mod` 10 == 0



-- B
fact :: Int -> Int
fact 0 = 0
fact x = x*x + fact (x - 1)

fact1 x = sum[n^2| n <- [1..x]]

fact2 = \x -> sum (map(\y -> y^2)[1..x])

-- D
toList :: Int -> [Int]
toList a = [digitToInt y | y <- show a]

toList2 :: Int -> [Int]
toList2 b = [ read [y] | y <- show b]

toList3 0 = []
toList3 x = toList(x`div`10) ++ [x `mod` 10]



-- 5.2
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

--5.3
square :: Int -> [(Int,Int)]
square a = [(x,y) | (x,y) <- grid a a, x /= y ]

--5.4

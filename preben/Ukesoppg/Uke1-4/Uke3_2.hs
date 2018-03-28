module Uke3_2 where

-- oppg 4.3
safetaila xs = if (null xs) then [] else tail xs

safetailb xs   | null xs   = []
               | otherwise  = tail xs

safetailc [] = []
safetailc xs = tail xs

(|||) :: Bool -> Bool -> Bool
False ||| False = False
_ ||| _ = True

(||||) :: Bool -> Bool -> Bool
False |||| True = True
True |||| False = True
True |||| True = True
False |||| False = False

(|||||) :: Bool -> Bool -> Bool
False ||||| a = a
True ||||| _ = True

(|||-) :: Bool -> Bool -> Bool
a |||- b  | a == b    = a
          | otherwise = True

--Oppg 4.5
(&&&) :: Bool -> Bool -> Bool
a &&& b = if a == b then if b == True then True else False else False

--Oppg 4.7
mult = \x -> \y -> \z -> x*y*z

-- Oppg B
--enklest
f 0 = 0
f x = x^2 + f (x-1)

-- liste komprehensjon
f' :: Int -> Int
f' x = sum [a^2 | a <- [1..x]]

-- lamda
f2 = \x -> x^2 + f (x-1)

-- Oppg C
luhnDouble :: Int -> Int
luhnDouble x | a > 9     = a - 9
             | otherwise = a
    where a = 2*x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn t x y z = a `mod` 10 == 0
    where a = luhnDouble t + x + luhnDouble y + z

-- Oppg. D
--enklest
toList' :: Int -> [Int]
toList' 0 = []
toList' x = toList' (x `div` 10) ++ [(x `mod` 10)]

--Listekomprehensjon
toList :: Int -> [Int]
toList x = [read [a] :: Int | a <- show x ]

-- Oppg E
-- 5.2
grid x y = [ (a,b) | a <- [0..x], b <- [0..y]]

--5.3
square x = [(a,b) | a <- [0..x], b <- [0..x], b /= a]

--5.4
replicate' n x = [x | _ <- n]
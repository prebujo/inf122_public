module Uke1_2 where
import Test.QuickCheck

-- UKE 1
-- Oppg A 1.1
-- double (double 2)
-- {applying double}

product1 :: [Int] -> Int
product1 (x:xs) = x * product xs

qsortrev :: [Int] -> [Int]
qsortrev [] = []
qsortrev (x:xs) = qsortrev larger ++ [x] ++ qsortrev smaller
                  where
          larger  = [a | a <- xs, a > x]
          smaller = [b | b <- xs, b < x]

double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

n = a `div` length xs
    where
      a = 10
      xs = [1,2,3,4,5]

last' xs = head (reverse xs)

last'' xs = xs !! (length xs -1)

init' xs = take (length xs - 1) xs

init'' xs = reverse (drop 1 (reverse xs))

-- Oppg B 1
plu :: [Int] -> Int -> [Int]
plu [] n = []
plu (k:ks) n = (k+n):plu ks n

-- Oppg B 2
pali :: String -> Bool
pali s = (s == reverse s)

-- Oppg B 3
pluTest [] n = True
pluTest ks n = plu ks n == map (+n) ks

pluCheck = quickCheck (\li -> \n -> pluTest li n)

paliTest :: String -> Bool
paliTest s = pali s == (s == reverse s)

paliCheck = quickCheck (\li -> paliTest li)

-- Oppg C

revLists :: [[a]] -> [[a]]
revLists [] = []
revLists (x:xs) = (reverse x) : revLists xs

revLists' = map reverse

revLists'' = foldl ( (:) (reverse )) []

-- Oppg D
del :: Int -> [Int]
del n = [a | a <- [1..n], mod5 a || mod3 a]
    where
        mod5 a = (a `mod` 5) == 0
        mod3 a = (a `mod` 3) == 0

dell :: Int -> [Int] -> [Int]
dell n k = [a | a <- [1..n], mod' a k]

mod' :: Int -> [Int] -> Bool
mod' a [] = False
mod' a (k:ks) = (a `mod` k == 0) || (mod' a ks)

data BR = (Content) | [Content] | BR BR
data Content = BR |
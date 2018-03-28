module Uke4_2 where
import Data.Char

--Oppg A
sub :: Int -> Int -> [Char] -> [Char]
sub a b xs = [c | (i,c) <- zip [0..] xs, i <= b-1 && i >= a]

sub' a b xs = take (b-a) (drop a xs)
sub1 a b xs = [x | (x, i) <- zip xs [0..length xs -1], i >= a && i < b]
sub2 a b xs = [xs !! n | n <- [a .. b-1]]

-- Oppg B
fjern xs s = [x | x <- xs, x /= s]

fjern' [] s = []
fjern' (x:xs) s = if (x == s) then fjern' xs s else x : (fjern xs s)

fjern2 xs s = filter (/=s) xs --også egentlig listekomprehensjon

-- Oppg C
tegnpos :: Char -> String -> [Int]
tegnpos c s = [ i | (i,a) <- zip [0..] s, a == c]

-- Oppg D
-- 6.4
euclid :: Int -> Int -> Int
euclid x y | x == y     = x
           | x > y      = euclid (x-y) y
           | x < y      = euclid x (y-x)

-- 6.5
-- length [1,2,3]
-- {applying length}
-- = 1 + length [2,3]
-- {applying length}
-- = 1 + 1 + length [3]
-- {applying length}
-- = 1 + 1 + 1 + length []
-- {applying length}
-- = 1 + 1 + 1 + 0
-- {applying +}
-- = 3

-- osv

-- 6.6
--a
and1 :: [Bool] -> Bool
and1 (x:xs) = x && and xs

--b
concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (x:xs) = x ++ concat xs

concat2 xs = foldl (++) [] xs

--c
replicate1 :: Int -> a -> [a]
replicate1 0 x = []
replicate1 n x = x : replicate1 (n-1) x

--d
(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x
(x:xs) !!! n = xs !!! (n-1)

--e
elem' :: Eq a => a -> [a] -> Bool
elem' y [] = False
elem' y (x:xs) | y==x    = True
               | otherwise = elem y xs

--Oppg E
clean :: String -> String
clean [] = []
clean (x:xs) | isAlpha x    = toLower x : clean xs
             | otherwise    = clean xs

-- Oppg F
tokenize :: String -> String -> String -> [String]
tokenize [] _ _ = []
tokenize (cha:chas) imp rem | cha `elem` imp    = [cha] : tokenize chas imp rem
                            | cha `elem` rem    = tokenize chas imp rem
                            | otherwise         = cha : tokenize chas imp rem


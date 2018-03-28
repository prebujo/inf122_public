
module Uke4 where
import Data.Char

sub :: Int -> Int -> [a] -> [a]
-- sub a b xs = [x | x <- [a..b], 0 <= a, a < b, b <= length xs]
sub a b xs = take (b-a) (drop a xs)
sub1 a b xs = [x | (x, i) <- zip xs [0..length xs -1], i > a && i < b]
sub2 a b xs = [xs !! n | n <- [a .. b-1]]

fjern :: String -> Char -> String
fjern xs y = [x | x <- xs, x /= y]
fjern1 xs y = filter xs /= y
-- fjern xs y =

-- fjern1 :: String -> Char -> String
-- fjern1 xs y =

-- tegnpos :: Char -> String -> [Int]
-- tegnpos a xs = [y | y <- [0 .. length xs -1], xs !! y == a]

--6.4
euclid :: Int -> Int -> Int
euclid a b | a==b        = b
           | a > b       = euclid(a-b) b
           | a < b       = euclid(b-a) a

--6.6
and1 :: [Bool] -> Bool
and2 [x] = x
and2 (x, xs) = x && and2 xs

and1 (x,xs) | x == False     = False
            | x == True    = and1 xs

concat1 :: [[a]] -> [a]
concat1 x = x
concat1 (x,xs) = x ++ concat1 xs

concat2 xs = foldl (++) [] xs
concat2 [] = []
concat2 (x,xs) = x ++ concat xs

[] acc = acc
concat3' (x:xs) acc = concat3' xs (acc ++ x)
concat3' xs = concat3' xs []



replicate :: Int -> a -> [a]
replicate 0 x = [x]
replicate a x = [x]: replicate (a-1) x --ikke halerekursiv! bruk accumulator (se over)

(!!!) :: [a] -> Int -> [a]
(!!!) [] a = error ""
(!!!) (x:xs) 0 = x
(!!!) (x:xs) a = (!!!) xs (a-1)

elem :: Eq a => a -> [a] -> Bool
elem x (y:ys) | x /= y      = False
              | x == y      = elem x ys

elem2 a [] = False
elem2 a (x:xs) | a == x   =True
               |otherwise =elem a xs

clean :: String -> String
clean [] = []
clean (x:xs) | isAlpha x   = toLower x : clean xs
             | otherwise   = clean xs

clean1 a | a == ""                      = ""
         | isAlpha(head a) == False     = "" ++ clean1 (tail a)
         | otherwise = toLower(head a) : clean1(tail a)

clean2 xs = clean' xs []
    where
     clean' [] acc = acc
     clean' (x:xs) acc | isAlpha x   = clean' xs (acc ++ [toLower x])
                       | otherwise   = clean' xs acc


tokenize :: String -> String -> String -> [String]
tokenize [] imp rem = []
tokenize str@(x:xs) imp rem
            | elem x imp = [x] ++ tokenize xs imp rem
            | elem x rem = tokenize xs imp rem
            | otherwise =
                    let (word, rest) = span(\c -> notElem c imp && notElem c rem) str -- evtl (imp ++ rem) istedet for &&
                    in word ++ tokenize rest imp rem


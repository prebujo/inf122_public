module Uke2_2 where
import Test.QuickCheck

--Oppg 0.
-- 3.2
bools = [False, True]

nums = [[3,2], [2,100]]

add x y z = x + y + z

copy = \x -> (x,x)

apply f x = f x

-- Oppg 3.3

-- :: [a] -> a
second xs = head (tail xs)

-- :: (a,b) -> (b,a)
swap (x,y) = (y,x)

-- :: a -> b -> (a,b)
pair x y = (x,y)

-- :: Num a => a -> a
double x = x*2

-- :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

-- :: (a -> a) -> a -> a
twice f x = f (f x)

-- Oppg 3.5
-- Functions may be depending on types or not and two diferent functions
-- may produce the same result although they have different types and visa-versa
-- checking for equality would mean to check that functions give the same result
-- for the same type of value input and this makes comparing functions
-- not an easy task as some functions have near infinite values of input.
-- However for small number of input (suh as with bool) it is feasable.


-- Oppg 2
--e1 :: [Bool]
e1 = [False, True, False]
--e2 :: [[Integer]]
e2 = [[1,2],[3,4]]
-- e3 :: [([Char],Integer)]
e3 = [ ("a",7) ]
--e4 :: [(Char, Integer)]
e4 = [ ('a',7) ]
--e5 :: Num a => a -> a
e5 x = x * 2

-- e6 :: (a, b) -> a
e6 (x,y) = x

e7 :: a -> (a,a)
e7 x = (x,x)

-- Oppg 3
-- [a] -> a  = head
-- [a] -> Int  = length

-- Oppg 4
-- foo1 :: a -> b -> (a,b)
foo1 a b = (a, b)
-- foo2 :: a -> b -> (a,b)
foo2 a = \b -> (a, b)
-- foo1 :: a -> b -> (a,b)
foo3 = \a b -> (a, b)
-- foo4 :: a -> b -> (a,b)
foo4 = \a -> \b -> (a, b)

-- Oppg 5
f :: Int -> Int -> Int
f x y = x + y
g :: (Int, Int) -> Int
g (x,y) = x + y

fgTest x y = f x y == g (x,y)

fgCheck = quickCheck (\x -> \y -> fgTest x y)
-- OK, passes 100 tests
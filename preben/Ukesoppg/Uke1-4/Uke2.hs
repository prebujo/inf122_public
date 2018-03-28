module Uke2 where
-- 3.1
-- ['a', 'b', 'c'] :: [Char]
-- ('a', 'b', 'c') :: (Char, Char, Char)
-- [(False, '0'),(True,'1')] :: [(Bool, Char)]
-- ([False, True],['0','1']) :: ([Bool], [Char])
-- [tail, init, reverse] :: [[a] -> [a]]

-- 3.2
bools = [False, True]
nums = [[2,3], [3,5]]
add x y z = x+y+z
copy x = (x,x)
apply f x = f x

-- 3.3
second xs = head (tail xs)      --second :: [a] -> a
swap (x,y) = (y,x)              --swap :: (t1, t) -> (t, t1)
pair x y = (x,y)                --pair :: t -> t1 -> (t, t1)
double x = x*2                  --double :: Num a => a -> a
palindrome xs = reverse xs == xs--palindrome :: Eq a => [a] -> Bool
twice f x = f (f x)             --twice :: (t -> t) -> t -> t

-- 3.4
-- see above

-- 3.5 ???
func1 y = y + y
func2 x = x + x
func3 z = z + 10
func4 u = 2 + 20
e2 = [[1,2],[3,4]]
e1 = [False, True, False]
e3 = [("a",7)]
e4 = [ ('a',7) ]
e5 :: Num a => a -> a
e5 x = x * 2
e6 (x,y) = x
e7 x = (x,x)

-- 4
foo1 a b = (a, b)
foo2 a = \b -> (a, b)
foo3 = \a b -> (a, b)
foo4 = \a -> \b -> (a, b)

--5
f :: Int -> Int -> Int
f x y = x + y

test3 :: (a -> b) -> c
f (y x) = /x -> y
test4 :: a -> (b -> c)
g x y = x

g :: (Int, Int) -> Int
g (x,y) = x + y
module ExamE where
lengthsum :: (Num a, Num b) => [a] -> (b,a)
lengthsum [] = (0,0)
lengthsum xs = (foldr (\x -> (+) 1) 0 xs, foldr (+) 0 xs)

lengthsum' xs = foldr (\ n (x, y) -> (1+x, n+y)) (0, 0) xs

inList :: (Eq a) => a -> [a] -> Bool
inList a = foldl (\ x y -> x || a == y) (False)

-- (((False || True) || False)||False)

--c)  foldr (:) "hello" "world!"
--    applying foldr = (:) "w"(foldr (:) "hello" "orld!")
--    applying foldr = (:) "w"((:) "o"(foldr (:) "hello" "rld!"))
--    applying remaining foldr = (:) "w"((:) "o"((:) "r"(...("hello"))))
--    applying (:) = "world!hello"

-- d) foldl (\ xs -> \ x -> x:xs) "INF122" "exam"
-- applying foldl = foldl (\ xs -> \x -> x:xs) ( (\ xs -> \x -> x:xs) "INF122" 'e') "xam"
-- inner func = foldl (\ xs -> \x -> x:xs) ( "eINF122") "xam"
-- applying foldl = foldl (\ xs -> \x -> x:xs) ( (\ xs -> \x -> x:xs) "eINF122" 'x') "am"
-- applying the rest of func/fold : = foldl (\ xs -> \x -> x:xs) ("maxeINF122") []
-- applying fold = "maxeINF122"

data Expr = V Int | M Expr Expr | D Expr Expr

eval :: Expr -> Maybe Int
eval (V a) = Just a
eval (M a b) = case eval a of
     Nothing -> Nothing
     Just n -> case eval b of
          Nothing -> Nothing
          Just m -> if (n >= 0 && m >= 0)
             then Just (n*m) else Nothing
eval (D a b) = case eval a of
      Nothing -> Nothing
      Just n -> case eval b of
          Nothing -> Nothing
          Just m -> if(n /= 0 || m /= 0)
              then Just (n `div` m) else Nothing

data Month = January | February | March | April

numDays :: Month -> Integer -> Integer
numDays a x = case a of
      January -> 31
      February -> if ((x `mod` 4) == 0 ) then 29 else 28
      March -> 31
      April -> 30

toDoList :: [IO a] -> IO [a]
toDoList [] = return []
toDoList (x:xs) = do
    v <- x
    vs <- toDoList xs
    return (v : vs)

mapActions :: (a -> IO b) -> [a] -> IO [b]
mapActions f (x:xs) = do
      v <- f x
      vs <- mapActions f xs
      return (v:vs)


sub x y xs = [a | (a,b) <- zip xs [0..], b >= x && b <y]

fjern :: String -> Char -> String
fjern str c = filter (/= c) str

fjern1 str c = [a | a <- str, a /= c]





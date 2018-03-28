module EksD where
-- Problem 2


--data BR = B | A | BR BR deriving Ord

--instance Eq BR where
--    (==) (B _) (B _) = True

append :: [a] -> [a] -> [a]
append x y = foldr (\x -> \y -> (++) y [x]) y x

append' x y = foldl (\x -> \y -> y:x) y x

-- Svar b) første runde gir 4/2 + 2 = 4.0
-- andre runde: 4/2 +4 = 6.0
-- tredje runde: 6/2 + 6 = 9.0


--Digresjon
del :: Int -> [Int]
del x = filter (modu) [1..x]
    where
      modu x = x `mod` 3 == 0 || x `mod` 5 == 0


dell :: Int-> [Int] -> [Int]
dell x ys = filter (modu) [1..x]
    where
      modu y = or [ y `mod` a == 0 | a <- ys]

-- Problem 3
type Name = String
type Pnum = Integer
type Pbook = Name -> Maybe Pnum

lookup' :: Pbook -> Name -> Maybe Pnum
lookup' b x = b x

insert :: Pbook -> Name -> Pnum -> Pbook
insert b n p | length (show p) /= 8    = error "Incorrect phone number"
             | otherwise        = \x -> if (x ==n) then Just p
                                          else b x


tokenize :: String -> String -> String -> [String]
tokenize [] _ _ = []
tokenize (x:xs) imp rem | elem x imp    = [x] : tokenize (xs) imp rem
                        | elem x rem    = tokenize (xs) imp rem
                        | otherwise     = taken : tokenize rest imp rem
                        where
                          (taken,rest) = span (\y -> not(elem y imp || elem y rem) ) (x:xs)



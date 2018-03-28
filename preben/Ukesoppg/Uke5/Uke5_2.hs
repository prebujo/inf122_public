module Uke5_2 where
import Data.Char

type Bit = Int

bintoint :: [Bit] -> Int
bintoint = foldr (\x y -> x + 2*y) 0

inttobin :: Int -> [Bit]
inttobin 0 = []
inttobin x = (x `mod` 2) : inttobin (x `div` 2)

--make8 :: [Bit] -> [Bit]
--make8 bits = take 8 (bits ++ repeat 0)
make9 :: [Bit] -> [Bit]
make9 bits = if (odd (sum bitpart)) then bitpart ++ [1]
                else bitpart ++ [0]
          where bitpart = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make9 . inttobin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = if (checkbin bits) then take 8 bits : chop8 (drop 9 bits)
              else error "incorrect binary sequence"

checkbin :: [Bit] -> Bool
checkbin bits | odd (sum(binnum)) && nine == 1   = True
              | even (sum(binnum)) && nine == 0  = True
              | otherwise                        = error "invalid bitcode"
        where binnum = take 8 bits
              nine = bits !! 8


decode :: [Bit] -> String
decode = map (chr.bintoint) . chop8

--Oppg B
--8.5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val a) = f a
folde f g (Add a b) = g (folde f g a) (folde f g b)

--8.6
eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (\x -> 1) (+)

-- Oppg C a
data Exp = S | U | Og Exp Exp | El Exp Exp | Ik Exp
folde' :: a -> a -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> Exp -> a
folde' s _ _ _ _ (S) = s
folde' _ u _ _ _ (U) = u
folde' s u og el ik (Og a b) = og (folde' s u og el ik a) (folde' s u og el ik b)
folde' s u og el ik (El a b) = el (folde' s u og el ik a) (folde' s u og el ik b)
folde' s u og el ik (Ik a) = ik (folde' s u og el ik a)

-- Oppg C b
evb :: Exp -> Bool
evb = folde' True False (&&) (||) (not)

-- Oppg C c
evi :: Exp -> Int
evi = folde' 1 5 (+) (*) (\x -> -x)

-- Oppg C d)
evh :: Exp -> Int
evh = folde' 1 1 (\x y -> max x y +1) (\x y -> max x y +1) (\x -> x + 1)


-- Oppg D
type Ordbok n k = [(n,k)]

finn :: Eq n => n -> Ordbok n k -> k

finn n [] = error "ord ikke i ordbok, bruk settInn"
finn n (ob:obs) | fst ob == n    = snd ob
                | otherwise      = finn n obs

endre :: Eq n => n -> v -> Ordbok n v -> Ordbok n v
endre n v [] = []
endre n v (ob:obs) | fst ob == n    = (n,v):obs
                   | otherwise      = ob : (endre n v obs)

slettAlle :: Eq n => n -> Ordbok n v -> Ordbok n v
slettAlle n ob = [(n',v) | (n',v) <- ob, n' /= n]

slettF :: Eq n => n -> Ordbok n v -> Ordbok n v
slettF n [] = []
slettF n (ob:obs) | fst ob == n   = obs
                  | otherwise     = ob : (slettF n obs)

erLike :: Eq n => Eq v => Ordbok n v -> Ordbok n v -> Bool
erLike [] _ = False
erLike _ [] = False
erLike ob1 ob2 | head ob1 == head ob2  = erLike (tail ob1) (tail ob2)
               | otherwise             = False

erLike2 :: Eq n => Eq v => Ordbok n v -> Ordbok n v -> Bool
erLike2 ob1 ob2 | length ob1 /= length ob2      = False
                | otherwise                     = like ob1 ob2 && like ob2 ob1
                where like a b = all (\x -> elem x a) b
module Uke5del4 where

type Ordbok n v = [(n, v)]

finn :: Eq n => n -> Ordbok n v -> v
finn n [] = error "Finner ikke ord."
finn nokl (par: ordb)
    | nokl == fst par     = snd par
    | otherwise           = finn nokl ordb

finn2 :: Eq n => n -> Ordbok n v -> v
finn2 n [] = error "Finner ikke ord.."
finn2 n t = head [v | (n', v) <- t, n == n']

settInn :: Eq n => n -> v -> Ordbok n v -> Ordbok n v
settInn n v ordb = (n,v) : ordb

endre :: Eq n => n -> v -> Ordbok n v -> Ordbok n v
endre _ _ [] = []
endre a b (par : ordb)
  | a == fst par  = (a,b) : ordb
  | otherwise     = par : (endre a b ordb)

slettF :: Eq n => n -> Ordbok n v -> Ordbok n v
slettF _ [] = []
slettF n (par:ordb)
  | n == fst par    = ordb
  | otherwise       = par : slettF n ordb

slettA :: Eq n => n -> Ordbok n v -> Ordbok n v
slettA n ordb = filter (\par -> n /= fst par) ordb

--erLike :: Eq n => Eq v => Ordbok n v -> Ordbok n v -> Bool
--erLike [] [] = True
--erLike (par:ordb) [] = False
--erLike (par:ordb) (par2:ordb2)
--  | par == par2     = erLike ordb (par2:ordb2)
--  | otherwise       = erLike (par:ordb) ordb2

erLike :: Eq n => Eq v => Ordbok n v -> Ordbok n v -> Bool
erLike ordb ordb2
  | length ordb /= length ordb2   = False
  | otherwise                     = subset ordb ordb2 && subset ordb2 ordb
      where subset a b = all (\x -> elem x a) b


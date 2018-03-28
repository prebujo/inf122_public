module Uke8 where

--Oppg 16.5
--formula: take n xs ++ drop n xs = xs
--Base Case:
-- take 0 xs ++ drop 0 xs =   {applying take}
-- [] ++ drop 0 xs =          {applying drop}
-- [] ++ xs =                 {applying ++}
-- xs
--Base Case 2:
-- take (n+1) [] ++ drop (n+1) [] =     {applying take/drop}
-- [] ++ [] =                           {applying ++}
-- []
-- Inductive case:
--take (n+1) (x:xs) ++ drop (n+1) (x:xs) =    {applying take/drop}
--(x:(take n xs) ++ drop n xs =               {applying ++}
-- x:(take n xs ++ drop n xs) =                {induction hypothesis}
-- x:xs

--Oppg 16.6
--data Tree = Leaf Int | Node Tree Tree
-- leaves (Leaf _) = 1
-- leaves (Node l r) = leaves l + leaves r
-- nodes (Leaf _) = 0
-- nodes (Node l r) = 1 + nodes l + nodes r

-- Prop: leaves t = nodes t + 1

-- Base case:
-- nodes (Leaf n) + 1 =       {applying nodes}
-- 0 + 1=                     {applying +}
-- 1 =                        {unapplying leaves}
-- leaves(Leaf n)

--Inductive case:
-- nodes (Node l r) + 1 =      {applying nodes}
-- 1 + nodes l + nodes r + 1   {arithmetics}
-- (nodes l + 1) + (nodes r + 1) {induction hyp}
-- leaves l + leaves r         {unapplying leaves}
-- leaves (Node l r)

--Oppg B
-- Hypothesis:
-- foldr f v [x1,x2,…,xn]  =  f x1 (f x2 (… (f xn v) …))

-- foldr:
-- foldr _ v [] = v
-- foldr f v (x:xs) = f x (foldr f v xs)


-- Inductive Case:
-- foldr f v (x:xs) =       {applying foldr}
-- f x (foldr f v xs)       {inductive hyp}
-- f x ( f xs1 (f xs2 (… (f xsn v) …)))


--Oppg C1
bsum:: Fractional t => Int -> t
bsum 0 = 0.0
bsum n = 1.0/(f*(1.0+f)) + bsum (n-1) where
    f = realToFrac n

-- Oppg C2
--Base Case:
-- bsum 1
-- {applying bsum}
-- 1.0/(1.0*(1.0 + 1.0)) = 1/2 -> holder

-- bsum (1+1)
-- {applying +}
-- = bsum 2
-- {applying bsum}
-- = 1.0/(2.0*(2.0+1.0)) + bsum 1
-- {applying bsum}
-- = 0.1666 + 0.50 = 0,6666 = 2/3
-- Fungerer for base case n og og n+1 fungerer dermed for alle videre n

-- Oppg C3
--Bevise at 1/(1*2) + 1/(2*3) + 1/(3*4) +… + 1/(n*(n+1))  =  n/(n+1)
--Base Case:
-- bsum 1
-- {applying bsum}
-- = 1.0/(1.0*(1.0 + 1.0)
-- {applying *}
-- = 1.0/(1.0 + 1.0)

--Inductive case


-- Oppg C4

bsum' n = n/(n+1)

-- Oppg F
-- data Tree a =  Leaf a | Node (Tree a) a (Tree a)
-- fmap :: (a -> b) -> Tree a -> Tree b
-- fmap f (Leaf x) = Leaf (f x)
-- fmap (Node ve x ho) = Node (fmap f ve) (f x) (fmap f ho)
--
-- Prop1: fmap id = id
-- Prop: fmap (g . h) = (fmap g) . (fmap h)

-- Base case, prop 1:
-- fmap id (Leaf x) =     {applying fmap}
-- Leaf (id x) =          {applying id}
-- Leaf (x) =             {unapplying id}
-- id (Leaf x)

-- Inductive case, prop1:
-- fmap (Node ve x ho) =                      {applying fmap}
-- Node (fmap id ve) (id x) (fmap id ho) =    {applying IH}
-- Node ((id ve) (id x) (id ho)) =            {applying id}
-- Node (ve x ho) =                           {unapplying id}
-- id (Node ve x ho)

-- Base Care, prop 2:
-- fmap (g . h) (Leaf x) =            {induction hyp}
-- fmap g (Leaf x) . fmap h (Leaf x)    =             {applying }
-- Leaf (g (h x))
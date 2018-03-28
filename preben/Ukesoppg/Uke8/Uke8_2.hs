module Uke8_2 where
--Oppg A, 16.5
--Base case:
--    take 0 xs ++ drop 0 xs
-- {applying take}
-- = [] ++ drop 0 xs
-- {applying drop}
-- = [] ++ xs
-- {applying ++}
-- = xs
--
--Base case 2:
-- take n [] ++ drop n []
-- {applying take & drop}
-- = [] ++ []
-- {applying ++}
-- = []
--
--Inductive case:
-- take n (x:xs) ++ drop n (x:xs)
-- {applying take & drop}
-- = x : take (n-1) xs ++ drop (n-1) xs
-- {distributivity}
-- = x : (take (n-1) xs ++ drop (n-1) xs)
-- {applying induction hypothesis}
-- = x : xs

-- Oppg A, 16.6
-- countLeaves :: Tree -> Int
-- countLeaves Leaf n = 1
-- countLeaves Node l r = countLeaves l + countLeaves r

-- countNodes :: Tree -> Int
-- countNodes Leaf n = 0
-- countNodes Node l r = countNodes l + countNodes r + 1

-- countLeaves Tree = countNodes Tree + 1 is the induction hypothesis

-- Base Case:
-- countLeaves Leaf n
-- {applying countLeaves}
-- = 1
-- { unapplying +}
-- = 0 + 1
-- {unapplying countNodes}
-- = countNodes Leaf n + 1

-- Inductive case:
-- countLeaves Node l r
-- {applying countLeaves}
-- = countLeaves l + countLeaves r + 1
-- {applying induction}
-- = countNodes l + 1 + countNodes r + 1 + 1
-- {commutativity of + and applying 1 + 1}
-- = countNodes l + countNodes r + 2 + 1
-- {unapplying countNodes}
-- = countNodes Node l r + 1

-- Oppg B

-- Beviser likheten foldr f v [x1,x2,…,xn]  =  f x1 (f x2 (… (f xn v) …))
--foldr _ v [] = v
--foldr f v (x:xs) = f x (foldr f v xs)
--

-- Base case:
-- foldr f v []
-- {applying foldr}
--  v

-- Indictive case:
-- foldr f v (x:xs)
-- {applying foldr}
-- f x (foldr f v xs)
-- {applying induction}
-- f x (f xs1 (f xs2 (..(f xsn v)..))))
-- bevist

--Oppg C1
bsum :: Fractional t => Int -> t
bsum n = 1.0/(f*(f+1.0)) + bsum (n-1)
  where f = realToFrac n

-- Oppg C2
-- Bevis at bsum n = 1/1*2 + 1/2*3 + ... + 1/n*(n+1)
--Base Case:
-- bsum 1
-- {applying bsum}
-- 1.0/(1.0*(1.0 + 1.0)) = 1/1*2  -> ok

-- bsum (n+1)
-- {applying bsum}
-- = 1/((n+1)*((n+1) + 1)) + bsum n
-- {applying induction hyp}
-- = 1/((n+1)*((n+1)+1) + 1/1*2 + .. + 1/n*(n+1)
-- {applying arithmetic's}
-- = 1/1*2 ... 1/((n+1)*((n+1)+1))) -> bevist

-- Fungerer for base case n og og n+1 fungerer dermed for alle videre n

-- Oppg C3
--Bevise at 1/(1*2) + 1/(2*3) + 1/(3*4) +… + 1/(n*(n+1))  =  n/(n+1)
--Base Case: (n = 1)
-- bsum 1
-- {applying bsum}
-- = 1.0/(1.0*(1.0 + 1.0)
-- {applying *}
-- = 1.0/(1.0 + 1.0) = n/(n+1)

--Inductive case: (n = n+1
-- bsum (n+1)
-- {applying bsum}
-- = 1.0/((n+1)*((n+1) + 1.0))) + bsum n
-- {applying bsum}
-- = 1.0/((n+1)*((n+1) + 1.0))) + 1/(1*2) + 1/2*3 + ... 1/(n*(n+1))
-- {ind. hyp}
-- = n/n+1 + 1/((n+1)*((n+1) + 1))
-- {arithmetic's}
-- = (n (n+2) + 1) / (n+1)(n+2)
-- = (n² + 2n + 1) / (n+1)(n+2)
-- = (n+1)²/(n+1)(n+2) = (n+1)/(n+2) = (n+1)/((n+1) +1) = n/(n+1)

-- Oppg C4
-- bsum :: Fractual t => Int -> t
-- bsum n = f/(f+1.0)
--  where f = realToFrac n

-- Oppg D
sub :: Eq t => [t] -> [t] -> [t]
sub [] ys = []
sub (x:xs) ys      | elem x ys    = sub xs ys
                   | otherwise    = x:(sub xs ys)
-- Skal bevise at sub xs ys = xm der xm er en liste av xs uten  elemeter fra ys. Deler dette opp i to tilfeller der basistilfellet blir at
-- listen ikke inneholder elementet og induksjonstilfellet blir at listen inneholder x.

-- Base case:
-- empty list
-- sub [] ys
-- {applying sub}
-- = []

-- Inductive case 1:
-- x not in the list of ys
-- sub [x] ys
-- {applying sub}
-- = x:(sub [] ys)
-- {applying sub}
-- = x:([])
-- {applying :}
-- = [x]

-- Inductive case 2:
-- x in the list of ys
-- sub (x:xs) ys
-- {applying sub}
-- = sub xs ys
-- {IH}
-- = xm       (der xm er en liste basert på xs uten elementer fra ys. Listen inneholder altså ikke x!)
-- Bevist.

-- Oppg E
-- map :: (a -> b) -> [a] -> [b]
-- map f [] = []
-- map f (x:xs) = (f x) : map f xs
--
-- id x = x
-- map id xs = xs
-- hvis id x = x må også length (id x) = length x
-- length xs = n

-- Bevis at map id = id for alle lister
-- Base case:
-- length (map id [])
-- {applying map}
-- = length(id [])
-- {applying id}
-- = length([])
-- {applying length}
-- = 0

-- Inductive case:
-- length (map id (x:xs))
-- {applying map}
-- length (id x : (map id xs))
-- {applying id}
-- length (x : (map id xs))
-- {applying length}
-- 1 + length (map id xs)
-- {IH}
-- 1 + length(xs)
-- {applying length}
-- 1 + n
-- Bevist

-- Bevise at map (f . g) = (map f) . (map g)
-- For hver liste xs har vi map (f . g) xs = (map f) . (map g) xs
-- def til function composition gir (f . g) x = f (g x) som forenkler beviset til map (f.g) xs = map f (map g xs)
--
-- Base case: (tom liste)
-- map (f . g) []
-- {applying map}
-- = []
-- {unapplying map}
-- = map g []
-- {unapplying map}
-- = map f (map g [])

-- Inductive case: (x:xs)
-- map (f . g) (x:xs)
-- {applying map}
-- = (f . g) x : map (f.g) xs
-- {fuction comp}
-- = f (g x) : map (f . g) xs
-- {IH}
-- = f (g x) : map f (map g xs)
-- {unapplying map}
-- = map f (g x : (map g xs))
-- {unapplying map}
-- = map f ( map g (x : xs))
-- Bevist

-- Oppg F
-- data Tree a =  Leaf a | Node (Tree a) a (Tree a)
-- fmap :: (a -> b) -> Tree a -> Tree b
-- fmap f (Leaf x) = Leaf (f x)
-- fmap f (Node ve x ho) = Node (fmap f ve) (f x) (fmap f ho)
--
-- Bevis følgende IH:
-- 	1. fmap id = id
-- 	2. fmap (f . g) = (fmap f) . (fmap g)
-- forenklet ved function definision til
-- 2. (fmap f) . (fmap g) = fmap f ( fmap g) because f.g x = f(g x)

-- Vi har at id a = a

-- Bevis 1:
-- Base case:
-- fmap id (Leaf a)
-- {applying fmap}
-- = Leaf (id a)
-- = {applying id}
-- = Leaf a
-- {unapplying id}
-- = id (Leaf a)

-- Inductive case:
-- fmap id (Node ve x ho)
-- {applying fmap}
-- = Node (fmap id ve) (id x) (fmap id ho)
-- {applying IH}
-- = Node (id ve) ( id x) ( id ho)
-- {applying id}
-- = Node ve x ho
-- {unapplying id}
-- = id (Node ve x ho)

-- Bevis 2:
-- Base case:
-- fmap (f . g) Leaf a
-- {applying fmap}
-- = Leaf ((f . g) a)
-- {applying function definition}
-- = Leaf (f(g a))
-- {unapplying fmap}
-- = (fmap f (g (Leaf a)))
-- {unapplying fmap}
-- = (fmap f (fmap g (Leaf a))

-- Inductive case:
-- fmap (f.g) (Node ve x ho)
-- {applying fmap}
-- = Node (fmap (f.g) ve) ((f.g) x) (fmap (f.g) ho)
-- {IH}
-- = Node (fmap f( fmap g ve)) (f (g x)) (fmap f (fmap g ho)) ??
-- {unapplying fmap}
-- = fmap f (Node (g ve) (g x) (g ho)
-- {unapplying fmap}
-- = fmap f (fmap g (Node ve x ho))
-- -> Bevist
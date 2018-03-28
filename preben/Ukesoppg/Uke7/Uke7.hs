module Uke7 where

-- Oppg. 1 formel:
pro x y = x
-- type blir pro i ghci blir :: t1 -> t -> t1
-- Formelen pro med lambdauttrykk: \x -> \y -> x

--Hindler-Milner:
--E(θ | pro :: τ) = E(θ | \x -> \y -> x :: τ) =:t
--t4 x :: a | \y -> x :: b.................. τ = a -> b
--t4 x :: a | y :: c | x :: d ...b = c -> d, τ = a -> b
--t2 ....................a = d, b = c -> d, τ = a -> b

--Unifikasjon:
--a = d, b = c -> d,   τ = a -> b
--a = d, b = c -> d,   τ = d -> b
--a = d, b = c -> d,   τ = d -> c -> d

-- Oppg. 2 : pro 2
-- typen til pro 2 blir i ghci :: Num t1 => t -> t1
-- Formelen pro2 med lambdauttrykk: \c -> \y -> x
-- Typen til 2 er Num a

-- Hindler-Milner:
--E(θ | pro :: τ) = E(θ | \c -> \y -> x :: τ) =
--t4 c :: a | \y -> x :: b ............................. τ = a -> b
--t4 c :: a | y :: e | x :: d .............. b = e -> d, τ = a -> b
--t2 c :: a | y :: e | x :: a ....... a = d, b = e -> a, τ = a -> b
--t1 c :: a | y :: e | x :: Num a ... a = Num a, a = d, b = e -> d, τ = a -> b

--Unifikasjon:
--a = Num a, a = d. b = e -> d,          τ = a -> b
--a = Num a, Num a = d, b = e -> d,      τ = Num a -> b
--a = Num a, Num a = d, b = e -> d,      τ = d -> b
--a = Num a, Num a = d, b = e -> d,      τ = d -> e -> d
--siden 2 er Num er dette ekvivalent med τ = Num d => e -> d

--Oppg. 3
comp f g x = f (g x)
comp' = \f -> \g -> \x -> (f (g x)) --uttrykt med lambda
--typen til comp blir i ghci :: (t1 -> t) -> (t2 -> t1) -> t2 -> t
--Formelen i lambdauttrykk: \f -> \g -> \x -> f (g x)

-- Hindler-Milner:
--E(θ | comp :: τ) = E(θ | \f -> \g -> \x -> f (g x) :: τ) =
--t4 f :: a | \g -> \x -> f (g x) :: b ................................................... τ = a -> b
--t4 f :: a | g :: c | \x -> f (g x) :: d..................................... b = c -> d, τ = a -> b
--t4 f :: a | g :: c | x :: e | f (g x) :: h ..................... d = e -> h, b = c -> d, τ = a -> b
--t3 f :: a | g :: c | x :: e | f :: j -> h ...................... d = e -> h, b = c -> d, τ = a -> b
-- U f :: a | g :: c | x :: e | g x :: j.......................... d = e -> h, b = c -> d, τ = a -> b
--t2 ................................................. a = j -> h, d = e -> h, b = c -> d, τ = a -> b
--U t3 f :: a | g :: c | x :: e | g :: i -> j..................... d = e -> h, b = c -> d, τ = a -> b
--U    f :: a | g :: c | x :: e | x :: i ......................... d = e -> h, b = c -> d, τ = a -> b =
-- .................................e = i. c = i -> j, a = j -> h, d = e -> h, b = c -> d, τ = a -> b

--Unifikasjon
-- e = i. c = i -> j, a = j -> h, d = e -> h, b = c -> d,             τ = a -> b
-- e = i. c = i -> j, a = j -> h, d = i -> h, b = c -> d,             τ = a -> b
-- e = i. c = i -> j, a = j -> h, d = i -> h, b = (i -> j) -> d       τ = a -> b
-- e = i. c = i -> j, a = j -> h, d = i -> h, b = (i -> j) -> d       τ = (j -> h) -> b
-- e = i. c = i -> j, a = j -> h, d = i -> h, b = (i -> j) -> (i -> h)  τ = (j -> h) -> b
-- e = i. c = i -> j, a = j -> h, d = i -> h, b = (i -> j) -> (i -> h)  τ = (j -> h) -> (i -> j) -> i -> h

-- Oppg 4
--ghci : \x -> \y -> x y :: (r1 -> r) -> r1 -> r

-- Hindler-Milner:
--E(θ | \x -> \y -> x y :: τ) =
--t4 x :: a | \y -> x y :: b) ...................................................   τ = a -> b
--t4 x :: a | y :: c | x y :: d) ....................................... b = c-> d, τ = a -> b
--t3 x :: a | y :: c | x :: e -> d) .................................... b = c-> d, τ = a -> b
--U  x :: a | y :: c | y :: e) ......................................... b = c-> d, τ = a -> b
--t2 ........................................................a = e -> d, b = c-> d, τ = a -> b
-- ................................................. c = e , a = e -> d, b = c-> d, τ = a -> b

--Unifikasjon
--c = e , a = e -> d, b = c-> d         τ = a -> b
--c = e , a = e -> d, b = e-> d         τ = a -> b
--c = e , a = e -> d, b = e-> d         τ = (e -> d) -> b
--c = e , a = e -> d, b = e-> d         τ = (e -> d) -> e -> d

-- Oppg 5
--ghci : \x -> \y -> (x y) x :: infinite type!

-- Hindler-Milner:
-- E(θ | \x -> \y -> (x y) x :: τ) =
--t4 x :: a | \y -> (x y) x :: b) .....................................................   τ = a -> b
--t4 x :: a | y :: c | (x y) x :: d) ........................................ b = c -> d, τ = a -> b
--t3 x :: a | y :: c | (x y) :: e -> d) ..................................... b = c -> d, τ = a -> b
-- U x :: a | y :: c | x :: e) .............................................. b = c -> d, τ = a -> b
--t2 ................................................................. a = e, b = c -> d, τ = a -> b
--U t3 x :: a | y :: c | x :: f -> (e -> d)........................... a = e, b = c -> d, τ = a -> b
--U    x :: a | y :: c | y :: f ...................................... a = e, b = c -> d, τ = a -> b =
-- ......................................... c = f, a = f -> e -> d), a = e. b = c -> d, τ = a -> b

--Uifikasjon
-- f = c, a = f -> (e -> d), a = e, b = c -> d,              τ = a -> b
-- f = c, a = c -> (e -> d), a = e, b = c -> d,              τ = a -> b
-- f = c, a = c -> (e -> d), c -> (e -> d) = e, b = c -> d,  τ = c -> (e -> d) -> b -> NO occurs check failer for e..
-- f = c, a = c -> (e -> d), c -> (e -> d) = e, b = c -> d,  τ = c -> (e -> d) -> c -> d

-- feiltypet occurs check failer da e er er type av seg selv, gir infinite type..

-- Oppg. 6    samme som på forelesning..
-- ghci: \x -> \y -> x (y x) :: (r1 -> r) -> ((r1 -> r) -> r1) -> r
--
--Hindley-Milner:
--E(θ | \x -> \y -> x (y x) :: τ) =
--t4 x :: a | \y -> x (y x) :: b).............................................. τ = a -> b
--t4 x :: a | y :: c | x (y x) :: d)................................b = c -> d, τ = a -> b
--t3 x :: a | y :: c | x :: e -> d).................................b = c -> d, τ = a -> b
-- U x :: a | y :: c | (y x) :: e)..................................b = c -> d, τ = a -> b
--t2....................................................a = e -> d, b = c -> d, τ = a -> b
--Ut3 x :: a | y :: c | x :: g).........................a = e -> d, b = c -> d, τ = a -> b
-- U x :: a | y :: c | y :: g -> e).....................a = e -> d, b = c -> d, τ = a -> b
-- t2....................................................................................
-- t2................................c = g -> e, a = g, a = e -> d, b = c -> d, τ = a -> b

--Unifikasjon:
-- c = g -> e, a = g, a = e -> d, b = c -> d,                     τ = a -> b
-- c = g -> e, a = g, a = e -> d, b = (g -> e) -> d,              τ = a -> b
-- c = g -> e, a = g, g = e -> d, b = (g -> e) -> d,              τ = g -> b
-- c = g -> e, a = g, g = e -> d, b = ((e -> d) -> e) -> d,       τ = (e -> d) -> b
-- c = g -> e, a = g, g = e -> d, b = ((e -> d) -> e) -> d,       τ = (e -> d) -> ((e->d) -> e) -> d


-- Oppg 7
-- \x -> \y -> x y x samme som oppg 5.
-- ghci: \x -> \y -> x y x  == \x -> \y -> (x y) x :: infinite type
-- samme fremgangsmåte som oppg 5

-- Oppg 8
-- ghci: \x -> x (\y -> x y) :: typefeil
-- TO DO:
-- Hindley-Milner:
--E(θ | \x -> x (\y -> x y) :: t) =
--t4   x :: a | x (\y -> x y) :: b) ............................... t = a -> b
--t3   x :: a | \y -> x y :: c).................................... t = a -> b
-- U   x :: a | x :: c -> b)....................................... t = a -> b
--t2    ............................................... a = c -> b, t = a -> b
--U    x :: a | y :: d | x y :: e)..........c = d -> e, a = c -> b, t = a -> b
--U t3 x :: a | y :: d | y :: g)........... c = d -> e, a = c -> b, t = a -> b
--U    x :: a | y :: d | x :: g -> e)...... c = d -> e, a = c -> b, t = a -> b
--t2   ................. a = g -> e, d = g, c = d -> e, a = c -> b, t = a -> b
--
--Unifikasjon:
-- a = g -> e, d = g, c = d -> e, a = c -> b,                      t = a -> b
-- a = g -> e, d = g, c = d -> e, g -> e = c -> b,                 t = g -> e -> b
-- a = g -> e, d = g, c = g -> e, g -> e = c -> b,                 t = g -> e -> b
-- a = g -> e, d = g, c = g -> e, c = c -> b,                      t = g -> e -> b     NO occurs check failure i c


-- Oppg 9
-- ghci: \x -> \y -> \z -> (x z) (y z) :: (r2 -> r1 -> r) -> (r2 -> r1) -> r2 -> r
-- TO DO:
-- E(θ |  \x -> \y -> \z -> (x z) (y z) :: τ) =
--  t4 x :: a | \y -> \z -> (x z) (y z) :: b) ................................. τ = a -> b
--  t4 x :: a | y :: c | \z -> (x z) (y z) :: d)....................b = c -> d, τ = a -> b
--  t4 x :: a | y :: c | z :: e | (x z) (y z) :: f) ....d = e -> f, b = c -> d, τ = a -> b
--  t3 x :: a | y :: c | z :: e | y z :: g) ............d = e -> f, b = c -> d, τ = a -> b
--   U x :: a | y :: c | z :: e | x z :: g -> f) .......d = e -> f, b = c -> d, τ = a -> b
--  t3 x :: a | y :: c | z :: e | z :: h ...............d = e -> f, b = c -> d, τ = a -> b
--   U x :: a | y :: c | z :: e | y :: h -> g ..........d = e -> f, b = c -> d, τ = a -> b
--U t3 x :: a | y :: c | z :: e | z :: i ...............d = e -> f, b = c -> d, τ = a -> b
--   U x :: a | y :: c | z :: e | x :: i -> g -> f .....d = e -> f, b = c -> d, τ = a -> b
-- t2 .......a = i -> g -> f, e = i, c = g -> h, e = h, d = e -> f, b = c -> d, τ = a -> b

--Unifikasjon:
-- a = i -> g -> f, e = i, c = h -> g, e = h, d = e -> f, b = c -> d,             τ = a -> b
-- a = i -> g -> f, e = i, c = h -> g, e = h, d = e -> f, b = c -> d,             τ = (i -> g -> f) -> b
-- a = i -> g -> f, e = i, c = h -> g, i = h, d = i -> f, b = c -> d,             τ = (i -> g -> f) -> b
-- a = i -> g -> f, e = i, c = h -> g, i = h, d = i -> f, b = (g -> h) -> d,      τ = (i -> g -> f) -> b
-- a = i -> g -> f, e = i, c = h -> g, i = h, d = h -> f, b = (g -> h) -> d,      τ = (h -> g -> f) -> b
-- a = i -> g -> f, e = i, c = h -> g, i = h, d = h -> f, b = (g -> h) -> h -> f, τ = (h -> g -> f) -> b
-- a = i -> g -> f, e = i, c = h -> g, i = h, d = h -> f, b = (g -> h) -> h -> f, τ = (h -> g -> f) -> (g -> h) -> h -> f





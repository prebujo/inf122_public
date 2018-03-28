-- Preben Bucher-Johannessen, Studentnr: pjo047, Dato: 05.10.17
module Oblig1 where
import Data.Char


--declaration of datatypes given by task
data Ast = Nr Int | Sum Ast Ast | Mul Ast Ast | Min Ast | If Ast Ast Ast | Let String Ast Ast | Var String
    deriving (Eq, Show)

-- parse calls on parseExpr and checks if all variables are declared in expression. If variables are not declared it returns all undeclared variables.
parse :: String -> Ast
parse str = let ast = (fst (parseExpr  (words str))) ; check = checkV [] ast in
    if (null check) then ast
    else error ("The following variables have not been declared: " ++ show check)

--parseExpr
parseExpr :: [String] -> (Ast, [String])
parseExpr ("+":s)     = let (e1, r1) = parseExpr s;
                            (e2, r2) = parseExpr r1 in (Sum e1 e2, r2)
parseExpr ("*":s)     = let (e1, r1) = parseExpr s;
                            (e2, r2) = parseExpr r1 in (Mul e1 e2, r2)
parseExpr ("-":s)     = let (e1, r1) = parseExpr s in (Min e1, r1)
parseExpr ("if":s)    = let (e1, r1) = parseExpr s;
                            (e2, r2) = parseExpr r1;
                            (e3, r3) = parseExpr r2 in (If e1 e2 e3, r3)
parseExpr ("let":x:"=":s)   = let (e1, r1) = parseExpr s;
                                  (_, r2)      = parseExpr r1;
                                  (e3, r3)     = parseExpr r2 in (Let x e1 e3, r3)
parseExpr (x:s)
    | isDigit (head x) = (Nr (read x :: Int), s)
    | otherwise        = (Var x, s)

--foldeA folds the given operators with the parsing given in the task and calls recursively on itsself on subexpressions
foldeA :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> (a -> Bool) -> [(String, a)] -> Ast -> a
foldeA a s m n i v (Nr x)         = a (x)
foldeA a s m n i v (Sum x y)      = s (foldeA a s m n i v x) (foldeA a s m n i v y)
foldeA a s m n i v (Mul x y)      = m (foldeA a s m n i v x) (foldeA a s m n i v y)
foldeA a s m n i v (Min x)        = n (foldeA a s m n i v x)
foldeA a s m n i v (If b t f)
    | i (foldeA a s m n i v b)    = (foldeA a s m n i v t)
    | otherwise                   = (foldeA a s m n i v f)
foldeA a s m n i v (Let var x y)  = (foldeA a s m n i ((var, foldeA a s m n i v x):v) y)
foldeA a s m n i v (Var f)        = head [y | (x,y) <- v, x == f]

-- evi calls on foldeA and with a parsed string
evi :: String -> Int
evi str = foldeA id (+) (*) negate (== 0) [] (parse str)

-- same as evi only with other operators to foldeA
evb :: String -> Bool
evb str = foldeA odd (||) (&&) not id [] (parse str)

{- checks if there are undeclared variables of the type Var. For all Nr. expressions the function will only return
a blank [] string. any undeclared Var will return that var in a string.
-}
checkV :: [String] -> Ast -> [String]
checkV l (Nr _)       = []
checkV l (Sum x y)    = checkV l x ++ checkV l y
checkV l (Mul x y)    = checkV l x ++ checkV l y
checkV l (Min x)      = checkV l x
checkV l (If b t f)   = checkV l b ++ checkV l t ++ checkV l f
checkV l (Let var x y)= checkV (var:l) x ++ checkV (var:l) y
checkV l (Var var)    = if var `elem` l then [] else [var]




module Obligtest where
import Data.Char

data Ast = Nr Int | Sum Ast Ast | Mul Ast Ast | Min Ast | Var String
    deriving (Eq, Show)

parse :: String -> Ast
parse a = (fst (parseExpr1  (words a)))

parseExpr1 :: [String] -> (Ast, [String])

parseExpr1 s = let (a, z) = parseExpr2 s in
    if null z then (a,z)
    else if head(z) == "+" then
      let (c, rest) = parseExpr1 (tail z) in (Sum a c, rest)
    else (a,z)
parseExpr2 :: [String] -> (Ast, [String])
parseExpr2 s = let (a, z) = parseExpr4 s in
    if null z then (a,z)
    else if head(z) == "*" then
        let (c, rest) = parseExpr2 (tail z) in (Mul a c, rest)
      else (a,z)

parseExpr3 :: [String] -> (Ast, [String])
parseExpr3 ("-":s) = let (a,b) = parseExpr3 s in (Min a, b)
parseExpr3 s = parseExpr4 s

parseExpr4 :: [String] -> (Ast, [String])
parseExpr4 (x:s) = if (onlyDigits x) then (Nr (read x :: Int), s)
                  else error ("Syntaksfeil ved " ++ x)

onlyDigits x = takeWhile isDigit x == x
module Uke6C where
import Data.Char

main :: IO ()
main = do
    putStrLn ("Please enter a number (1-99) for the NxN Matrix followed by *Enter*: ")
    s <- getLine
    let n = read s :: Int
    let mat = makeMat n
    runprogram mat
    putStrLn("Programmet er avsluttet.")
--  putStrLn("Pretty neat right... what do u want to do next? wanna change a dot with an X ? give the row,column ;)")
--  playMat

type Matrix = [[Char]]

makeMat :: Int -> Matrix
makeMat n = [take n (repeat line) | line <- take n (repeat '.')]

runprogram :: Matrix -> IO()
runprogram mat = do
    putStrLn (stringBuild mat)
    putStrLn("Hva vil du gjøre nå? (d x y, erstatter node x,y med X, n x y setter noden til .) \n q *Enter* for å gå ut av programmet")
    ans <- getLine
    let form = words ans
        f = head form
    case f of
      "q" -> return()
      "d" -> do
             let x = (read(form !! 1) :: Int) -1
                 y = (read(form !! 2) :: Int) -1
                 mat' = func mat x y f
             runprogram mat'
      otherwise -> do
             runprogram mat


stringBuild :: Matrix -> String
stringBuild mat = "" ++ concat fstline ++"\n" ++ concat matTb
        where fstline = ["\t" ++ show num | num <- [1..(length(head(mat)))]]
              nextlines = [show row ++ matLn ++ "\n" | (row,matLn) <- zip [1..(length(mat))] mat]
              matTb = [tab elem | line <- nextlines, elem <- line]
              tab a
                | isDigit a  = [a]
                | otherwise  = "\t" ++ [a]

func:: Matrix -> Int -> Int -> [Char] -> Matrix
func (t:ts) x 0 f = (func' t x f):ts
func (t:ts) x y f = t: func ts x (y-1) f

func':: [Char] -> Int -> [Char] -> [Char]
func' (t:ts) 0 f   | f == "d"  = 'X':ts
                   | f == "n"  = '.':ts
func' (t:ts) y f = t: func' ts (y-1) f

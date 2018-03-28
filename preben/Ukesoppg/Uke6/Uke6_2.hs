module Uke6_2 where
import System.IO

adder :: IO ()
adder = do
        putStr("How many numbers? ")
        n <- getLine
        total <- sumInput (read n :: Int) 0
        putStr("The total is: ")
        putStrLn(show total)
        return()

sumInput :: Int -> Int -> IO Int
sumInput n tot = do
    if (n == 0) then return tot
    else do
      inp <- getLine
      sumInput (n-1) (tot + (read inp :: Int))
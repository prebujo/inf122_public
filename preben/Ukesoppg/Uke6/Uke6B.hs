module Uke6B where

main :: IO ()
main = do  putStrLn ("Please enter a number (1-99) for the NxN Matrix followed by *Enter*: ")
          n <- getLine
          let m = read n :: Int
          printM m (m+1)
          putStrLn("Pretty neat right... ;)")

printM :: Int -> Int -> IO ()
printM n m = do
    if (n - m == -1) then
      do
      printfstLn n n
      printM n (m-1)
    else if (m == 0) then
      return()
    else
      do
      printDotLn n n (m-1)
      printM n (m-1)

printfstLn :: Int -> Int -> IO()
printfstLn n m = do
    if (m == 0) then
      do
      putStrLn("")
      return ()
    else if (n-m < 10) then
      do
      putStr("  " ++ (show (n-m+1)))
      printfstLn n (m-1)
    else
      do
      putStr(" " ++ (show (n-m+1)))
      printfstLn n (m-1)

printDotLn :: Int -> Int -> Int -> IO()
printDotLn n m o = do
    if (m == 0) then
      do
      putStrLn ("  " ++ (show (n - o)))
      return()
    else
      do
        putStr ("  .")
        printDotLn n (m-1) o

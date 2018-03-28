module Nim where
import System.IO

main = do
    putStrLn("please a number: ")
    size <- readLn :: IO Integer
    if(10 < size && size < 20) then do
      putStrLn("your number was: " ++ show size)
      play size 1
    else do
        putStrLn("Please enter number between 10 and 20")
        main

play :: Integer -> Integer -> IO ()
play size p = do
    if(size <=3) then do
      putStrLn("Player " ++show p++" wins!")
      return ()
    else do
      putStrLn("Heap size "++show size++". Player "++show p++": remove 1 to 3  objects from heap.")
      take <- readLn :: IO Integer
      if(take <=3 && take >=1) then do
        play (size - take) (((p) `mod` 2) + 1)
      else do
        putStrLn("Please enter valid number!")
        play size p


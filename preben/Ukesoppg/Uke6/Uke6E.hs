module Uke6E where
import Data.Char

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Int -> Board
initial 0 = []
initial n = n:initial (n-1)

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
    where update r n = if r == row then n-num else n

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat(replicate num "* "))

--putBoard' :: Board -> IO ()
--putBoard' [a, b, c, d, e] = do putRow 1 a
--                              putRow 2 b
--                              putRow 3 c
--                              putRow 4 d
--                              putRow 5 e

putBoard :: Board -> Int -> IO()
putBoard [] n = return()
putBoard b 0 = return()
putBoard (t:ts) n = do putRow n t
                       putBoard ts (n-1)

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do putStrLn "ERROR: Invalid digit"
                           getDigit prompt

newline :: IO ()
newline = putChar '\n'

play :: Board -> Int -> Int -> IO ()
play board player size =
    do newline
       putBoard board size
       if finished board then
          do newline
             putStr "Player "
             putStr (show (next player))
             putStr " wins!!"
          else
            do newline
               putStr "Player "
               putStrLn (show player)
               putStrLn("Enter a valid move (row/stars) separated by space, 0 takes back last move")
               args <- getLine
               let cmd = words args
                   row = read (head cmd) :: Int
                   num = read (cmd !! 1) :: Int
               if (row == 0) then
                  moveBack
               else if valid board (size - (row-1)) num then
                  play (move board (size -(row-1)) num) (next player) size
               else
                  do newline
                     putStrLn "ERROR: Invalid move"
                     play board player size

nim :: IO ()
nim = do
  putStrLn ("Please enter a size of the board:")
  boardsz <- getLine
  let  size = read (boardsz) :: Int
       board = initial size
  play board 1 size
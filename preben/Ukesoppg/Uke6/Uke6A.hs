adder :: IO()
adder = do putStrLn "How many numbers? "
           amount <- getLine
           sumT <- sumA (read amount :: Int) 0
           putStrLn ("The total is " ++ sumT)

sumA :: Int -> Int -> IO String
sumA x y = do
              if(x == 0) then
                  return (show y)
              else
                  do  a <- getIntLn
                      z <- sumA (x-1) (a + y)
                      return z

getIntLn :: IO Int
getIntLn = do x <- getChar
              if x == '\n' then
                return 0
              else
                do xs <- getChar
                   return (read ([x]++[xs]) :: Int)

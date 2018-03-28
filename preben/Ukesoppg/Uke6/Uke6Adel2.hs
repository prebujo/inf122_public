adder :: IO()
adder = do putStrLn "Please put in as many numbers as you with but finish with a 0"
           sumT <- sumA 0
           putStrLn ("The total is " ++ (show sumT))

sumA :: Int -> IO Int
sumA x = do   m <- getLine
              if(m == "0") then
                  return x
              else
                  do  tot <- sumA (x + (read m :: Int))
                      return tot


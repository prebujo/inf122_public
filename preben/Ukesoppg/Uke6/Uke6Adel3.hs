adder :: IO()
adder = do putStrLn "Please put in as many numbers as you wish separated by *space* and finish by pressing *enter*. "
           sumT <- sumA 0
           putStrLn ("The total is " ++ (show sumT))

sumA :: Int -> IO Int
sumA (x) = do   m <- getNum
                if(m == "\n") then
                   return x
                else if (m == "") then
                    do  tot <- sumA (x)
                        return tot
                else
                    do  tot <- sumA (x + (read m :: Int))
                        return tot


getNum :: IO String
getNum = do m <- getChar
            if m == '\n' then
              return "\n"
            else if m == ' ' then
              return []
            else
              do ms <- getNum
                 return (m:ms)

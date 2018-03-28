module Life where

cls :: IO()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()

writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

type Board = ([Pos], Int)
type Rule = (Int, Int)
type Rules = (Rule,Rule)

glider :: Board
glider = ([(4,2), (2,3), (4,3), (3,4), (4,4)], 10)

showcells :: Board -> IO ()
showcells (b,n) = sequence_ [writeat p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive (b,n) p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty (b,n) p = not (isAlive (b,n) p)

neighbs :: Int -> Pos -> [Pos]
neighbs n (x,y) = filter (limit n) [(x-1,y-1), (x, y-1),
                                   (x+1,y-1), (x-1, y),
                                   (x+1, y),  (x-1, y+1),
                                   (x,y+1),   (x+1, y+1)]
limit :: Int -> Pos -> Bool
limit n (x,y) = x > 0 && x <= n && y > 0 && y <= n


liveneighbs :: Board -> Pos -> Int
liveneighbs (b,n) = length.filter (isAlive (b,n)) . (neighbs n)

survivors :: Board -> Rule -> [Pos]
survivors (b,n) (x,y) = [p | p  <- b, elem (liveneighbs (b,n) p) [x,y]]

births :: Board -> Rule -> [Pos]
births (b,n) (x,y) = [p | p <- rmdups (concat (map (neighbs n) b)),
                    isEmpty (b,n) p,
                    liveneighbs (b,n) p <= y,
                    liveneighbs (b,n) p >= x]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x:rmdups(filter(/=x) xs)

nextgen :: Board -> Board
nextgen (b,n) = (survivors (b,n) r1 ++ births (b,n) r2 , n)
    where r1 = (2,3)
          r2 = (3,3)

life :: Board -> IO ()
life b = do
            cls
            showcells b
            wait 500000
            life (nextgen b)

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]
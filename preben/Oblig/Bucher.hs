-- Preben Bucher-Johannessen
-- horisontal.hs: s 2 3 b 3 3 10 5 4 5 3 5 2 4 1 3 4 2 4 1 3 4 4

module Bucher where
import Data.Char

type Matrix = [[Char]]
type Board = ([Pos], Int)
type Pos = (Int, Int)
type Rule = (Int, Int)
type Rules = (Rule, Rule)


--main starts the program by calling runprogram on an empty board and standard Conways Game of life rules.
main :: IO ()
main = do
    putStrLn("Welcome to Game of Life. The Game is now started and you can specify what board u want with 'c'. ")
    putStrLn("Rules are standard Conways Game of Life. Change them with commands 's' and 'b'. ")
    runprogram ([], 0) ((2, 3), (3, 3))
    putStrLn("Programmet er avsluttet.")

runprogram :: Board -> Rules -> IO()
runprogram (b, n) ruls = do
    printBoard (b,n)
    putStrLn("What would you like to do? (type 'help' for commandlist)")
    ans <- getLine
    let form = words ans
        f = head form
    case f of
      "q" -> return()
      "help" -> do
        cls
        printcmdList
        runprogram (b,n) ruls
      "c" -> do
        let n2 = (read(form !! 1) :: Int)
        runprogram ([], n2) ruls
      "n" -> do
        let x = (read(form !! 1) :: Int) -1
            y = (read(form !! 2) :: Int) -1
            b' = expand (b,n) (x,y)
        runprogram b' ruls
      "d" -> do
        let x = (read(form !! 1) :: Int) -1
            y = (read(form !! 2) :: Int) -1
            b' = delete (b,n) (x,y)
        runprogram b' ruls
      "s" -> do
        let x = (read(form !! 1) :: Int)
            y = (read(form !! 2) :: Int)
            ruls' = surv ruls (x,y)
        runprogram (b,n) ruls'
      "b" -> do
        let x = (read(form !! 1) :: Int)
            y = (read(form !! 2) :: Int)
            ruls' = born ruls (x,y)
        runprogram (b,n) ruls'
      "?" -> do
        printRules ruls
        runprogram (b,n) ruls
      "w" -> do
        let f = form !! 1
        writeF f (b,n) ruls
        putStrLn("Board saved in " ++ f)
        runprogram (b, n) ruls
      "r" -> do
        let f = form !! 1
        (b', ruls') <- readF f
        runprogram b' ruls'
      "CR" -> do
        let b' = nextgen (b,n) ruls
        runprogram b' ruls
      "l" -> do
        let x = (read(form !! 1) :: Int)
        b' <- runGen x (b,n) ruls
        runprogram b' ruls
      otherwise -> do runprogram (b,n) ruls

printcmdList = do
  putStrLn("Command-list:")
  putStrLn(" q     -   quits the program")
  putStrLn(" c n   -   makes a Matrix of n*n open slots")
  putStrLn(" n x y -   places a cell (X) on slot x,y")
  putStrLn(" d x y -   empties slot x,y ")
  putStrLn(" s x y -   neighbours with [x..y] cells survives")
  putStrLn(" b x y -   empty slot with [x..y] neighbors becomes alive")
  putStrLn(" ?     -   show current games rules")
  putStrLn(" w n   -   write board and rules to filename n")
  putStrLn(" r n   -   read board and rules from file n ")
  putStrLn(" CR    -   move one generation forward with current board")
  putStrLn(" l x   -   play game x rounds (or until stable)")
  putStrLn("")
  putStrLn(" be responsible and dont let the cells take over....")
  putStrLn("")


--Write/Read File functions
writeF :: String -> Board -> Rules -> IO()
writeF name (pos, size) ((sl, sh), (bl, bh)) = do
  let str = "s " ++ show sl ++" " ++ show sh ++
            " b " ++ show bl ++ " " ++ show bh ++
            " " ++ show size ++ " " ++ posStr pos
  writeFile name str

posStr :: [Pos] -> String
posStr [] = ""
posStr (x:xs) = show (a+1) ++ " " ++ show (b+1) ++" " ++ posStr xs
    where
    (a,b) = x

readF :: String -> IO (Board, Rules)
readF name = do
    text <- readFile name
    let wlist = words text
    let board = (listToPos (drop 7 wlist), (read(wlist !! 6) ::Int))
    if ((head wlist) == "s") then do
      let ruls = (((read(wlist !! 1) ::Int), (read(wlist !! 2) ::Int)),
                 ((read(wlist !! 4) ::Int), (read(wlist !! 5) ::Int)))
      return (board, ruls)
    else do
      let ruls = (((read (wlist !! 4) ::Int), (read(wlist !! 5) ::Int)),
                 ((read(wlist !! 1) ::Int), (read(wlist !! 2) ::Int)))
      return (board, ruls)

listToPos :: [String] -> [Pos]
listToPos (x:y:[]) = [((read(x)::Int)-1 , (read(y)::Int)-1)]
listToPos (x:y:rest) = [((read(x) :: Int)-1,(read(y) :: Int)-1)] ++ listToPos rest

--Rules functions
surv :: Rules -> Rule -> Rules
surv (surv, born) r = (r, born)

born :: Rules -> Rule -> Rules
born (surv, born) r = (surv, r)

printRules :: Rules -> IO()
printRules ((sl, sh),(bl, bh)) = do
  putStrLn("*****Game of Life Rules***********")
  putStrLn("Survives: \t [" ++ show sl ++ ", " ++ show sh ++ "]")
  putStrLn("Born: \t\t [" ++show bl++ ", " ++ show bh ++ "]")
  return()


-- making the matrix/Board with Board as input and printing it to the user.
makeMat :: Int -> Matrix
makeMat n = [take n (repeat line) | line <- take n (repeat '.')]

printBoard :: Board -> IO()
printBoard ([],0) = do
    putStrLn ("Status: <<No board defined>> ")
printBoard (b,n) = do
    let m = makeMat n
        m' = boardToMat (b,n) m
    putStrLn(stringBuild m')

boardToMat :: Board -> Matrix -> Matrix
boardToMat ([], n) m = m
boardToMat ((b:bs),n) m = boardToMat (bs, n) (markX m b)


-- changing positions in Board with X
markX:: Matrix -> Pos -> Matrix
markX (t:ts) (x,0) = (markX' t x):ts
markX (t:ts) (x,y) = t: markX ts (x, (y-1))

markX':: [Char] -> Int -> [Char]
markX' (t:ts) 0 = 'X':ts
markX' (t:ts) y = t: markX' ts (y-1)

-- printing the board to the screen.
stringBuild :: Matrix -> String
stringBuild [] = ""
stringBuild mat = "" ++ concat fstline ++"\n" ++ concat matTb
        where fstline = ["\t" ++ show num | num <- [1..(length(head(mat)))]]
              nextlines = [show row ++ matLn ++ "\n" | (row,matLn) <- zip [1..(length(mat))] mat]
              matTb = [tab elem | line <- nextlines, elem <- line]
              tab a
                | isDigit a  = [a]
                | otherwise  = "\t" ++ [a]

-- Game of Life, basert på koden i boken med små tilpasninger

cls :: IO()
cls = putStr "\ESC[2J"

expand :: Board -> Pos -> Board
expand (b,n) p = (b++[p], n)

delete :: Board -> Pos -> Board
delete (b,n) p = (filter (/=p) b, n)

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
limit n (x,y) = x >= 0 && x <= (n-1) && y >= 0 && y <= (n-1)


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

nextgen :: Board -> Rules -> Board
nextgen (b,n) (surv, born) = ((survivors (b,n) surv) ++ (births (b,n) born) , n)


runGen :: Int -> Board -> Rules -> IO Board
runGen 0 b r = return b
runGen x b r = do
  cls
  printBoard b
  wait 1000000
  let nextb = nextgen b r
  if (nextb == b) then do
    putStrLn("Stabilt stadie funnet..")
    return b
  else runGen (x-1) (nextgen b r) r

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

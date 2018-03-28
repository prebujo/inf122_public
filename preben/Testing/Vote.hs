module Vote where
import Data.List

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Blue", "Red"]

--counts how many votes of the same type
count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

--ReMoves DUPlicateS from a list
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x :filter (/=x) (rmdups xs)

--sorterer en liste med resultatet av stemmingen
result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

--declares the winner of a voting
winner :: Ord a => [a] -> a
winner = snd . last . result

--declares a ballot list of list of strings
ballots :: [[String]]
ballots =  [["Red", "Green"],
            ["Blue"],
            ["Green", "Red", "Blue"],
            ["Blue", "Green", "Red"],
            ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
                [c]     -> c
                (c:cs)  -> winner' (elim c bs)





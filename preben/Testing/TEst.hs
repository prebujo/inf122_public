module TEst where
import Data.Char

clean :: String -> String
clean [] = []
clean (x:xs) | isAlpha x   = toLower x : clean xs
             | otherwise   = clean xs
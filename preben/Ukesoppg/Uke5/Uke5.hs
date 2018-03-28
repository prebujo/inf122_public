module Uke5 where
module Bit where
import Data.Char
type Bit = Int



-- Oppg 7.7 lag en kontrollbit
bin2int :: [Bit] -> Int
bin2int bits = sum[w*b | (w,b) <- zip weights bits]
               where weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make9 :: [Bit] -> [Bit]
make9 bits = take 8 (bits ++ repeat 0) ++ [oddInt bits]

oddInt :: [Bit] -> Int
oddInt bits | odd (sum bits)    = 1
            | otherwise         = 0

encode' :: String -> [Bit]
encode' = concat . map (make9 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits | (check9th firstBit)   = take 8 bits : chop9 (drop 9 bits)
           | otherwise          = error "At least one invalid Bitcode"
           where firstBit = take 9 bits

check9th :: [Bit] -> Bool
check9th xs | nineth == 1 && odd (sum (take 8 xs))        = True
            | nineth == 0 && even (sum (take 8 xs))       = True
            | otherwise                                 = error "Invalid Bitcode"
            where nineth = xs !! 8


decode' :: [Bit] -> String
decode' = map(chr . bin2int) . chop9




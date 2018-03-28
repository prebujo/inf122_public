module Oving where
data Ret = Op | Ne | Ve | Ho deriving (Eq, Ord, Read, Show)
type Pos = (Int, Int)
mv :: Pos -> Ret -> Pos
mv (x,y) Ve = (x-1,y)
mv (x,y) Ne = (x+1,y)
mv (x,y) Op = (x,y+1)
mv (x,y) Ho = (x,y-1)
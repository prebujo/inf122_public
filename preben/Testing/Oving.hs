module Oving where
data Ret = Op | Ne | Ve | Ho deriving (Eq, Ord, Read, Show)
type Pos = (Int, Int)
mv :: Pos -> Ret -> Pos
mv (x,y) Ve = (x-1,y)
mv (x,y) Ne = (x+1,y)
mv (x,y) Op = (x,y+1)
mv (x,y) Ho = (x,y-1)

type Pos = (Int, Int)
data Move = North | South | West | East deriving Show

move North (x,y) = (x, y+1)
move South (x,y) = (x, y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (x:xs) p = moves xs (move x p)

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East

moveRev [] p = p
moveRev (x:xs) p = moveRev xs (move (rev x) p)
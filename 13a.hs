import AOC
import Data.List
import Data.List.Split

data Axis = X | Y deriving (Show, Eq)



main = interactl $ solve . splitOn [""]

solve [ds, fs] = length $ nub $ f dots $ head dirs --f dots $ head dirs
  where
    dots = map (pairs  . splitOn ",") ds
    dirs = map (parseDirs . splitOn "=" . (!! 2) . words) fs
solve _ = -1


f ds (X,c) = map (fx c) ds
f ds (Y,c) = map (fy c) ds

fx c (x,y)
  | x > c = (c-(x-c), y)
  | otherwise = (x,y)

fy c (x,y)
  | y > c = (x, c-(y-c))
  | otherwise = (x,y)







parseDirs :: [String] -> (Axis, Int)
parseDirs [d, c]
  | d == "x" = (X, read c)
  | otherwise = (Y,read c)
parseDirs _ = (X, -1)

-- TODO(filip): Refactor (also in day 12) make work for any type - use maybe in an inter. function
-- can a functios type be influenced by the caller? - how does parse p work?
pairs :: [String] -> (Int,Int)
pairs [x, y] = (read x,read y)
pairs _ = (-1, -1)




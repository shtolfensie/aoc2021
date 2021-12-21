import AOC
import qualified Data.Vector as V
import Data.List
import Data.List.Split

data Axis = X | Y deriving (Show, Eq)



main = interactl' $ solve . splitOn [""]

solve [ds, fs] = draw (fst bs) rs
  where
    rs = nub $ foldl f dots dirs
    bs = findBounds rs
    dots = map (pairs  . splitOn ",") ds
    dirs = map (parseDirs . splitOn "=" . (!! 2). words) fs
solve _ = "Error"

findBounds dots = g (head dots) (head dots) dots
  where
    g (mx,my) (ux, uy) ((x,y):ps) = g maxPoint minPoint ps
      where
        maxPoint = (if x>mx then x else mx, if y>my then y else my)
        minPoint = (if x<ux then x else ux, if y<uy then y else uy)
    g maxP minP [] = (maxP, minP)

draw (x,y) dots = makeLines x x $ concat $ V.toList $ foldl (\a c -> a V.// [(indexMap x c, "#")]) (V.replicate ((x+1)*(y+1)) " ") dots

indexMap mx (x,y) = x + ((mx+1)*y)


f ds (X,c) = map (fx c) ds
f ds (Y,c) = map (fy c) ds

fx c (x,y)
  | x > c = (c-(x-c), y)
  | otherwise = (x,y)

fy c (x,y)
  | y > c = (x, c-(y-c))
  | otherwise = (x,y)


makeLines t 0 (d:ds) = d:'\n':makeLines t t ds
makeLines t c (d:ds) = d:makeLines t (c-1) ds
makeLines _ _ [] = ""






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




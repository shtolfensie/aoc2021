import AOC
import Data.List
import qualified Data.Vector as V


-- main = interactl $ f . rights . map (parse p)
main = print "not working"

type Point = (Int, Int)

type Cell = (Point, Int)

type Grid = (Int, V.Vector Int)

rights (Right x:xs) = x: rights xs
rights (_:xs) = rights xs
rights [] = []

p :: Parser (Point, Point)
p = do
  x1 <- many1 digit
  char ','
  y1 <- many1 digit
  string " -> "
  x2 <- many1 digit
  char ','
  y2 <- many1 digit
  return ((read x1, read y1), (read x2, read y2))

id' :: Int -> Int -> Int
id' a _ = a

eq' :: Point -> Point -> Bool
eq' a b = (fst a == fst b) && (snd a == snd b)

dir :: (Point, Point) -> ((Int -> Int -> Int), Int -> Int -> Int)
dir (a, b)
  | fst a - fst b < 0 = ((+), id')
  | fst a - fst b > 0 = ((-), id')
  | snd a - snd b < 0 = (id', (+))
  | snd a - snd b > 0 = (id', (-))
  | otherwise = (id', id')

apply :: ((Int -> Int -> Int), (Int -> Int -> Int)) -> Point -> Point
apply ops p = ((fst ops) (fst p) 1, (snd ops) (snd p) 1)

straightLines :: [(Point, Point)] -> [(Point, Point)]
straightLines = filter (\(a,b) -> fst a == fst b || snd a == snd b)

-- count :: (a -> Bool) -> [a] -> Int
-- count f = foldl (\a n -> if f n then a+1 else a) 0

count :: (a -> Bool) -> V.Vector a -> Int
count f = V.foldl (\a n -> if f n then a+1 else a) 0

maxPoint :: [(Point, Point)] -> Point
maxPoint ls = (mx, my)
  where
    ps = foldl (\a n -> fst n:(snd n:a)) [] ls
    xs = map fst ps
    ys = map snd ps
    mx = maximum xs
    my = maximum ys

indexMap :: Int -> Point -> Int
indexMap c (x,y) = x + (c*y)
  

f :: [(Point, Point)] -> Int
f ls = count (>=2) $ snd $ foldl' markLine (fst mp, (V.replicate ((fst mp +1) * (snd mp +1)) 0)) $ straightLines ls
  where
    mp = maxPoint ls
-- count (\(_,c) -> c>=2) $ 

markLine :: Grid -> (Point, Point) -> Grid
markLine g l = move ops g l
  where
    ops = dir l

move :: ((Int -> Int -> Int), (Int -> Int -> Int)) -> Grid -> (Point, Point) -> Grid
move ops g (a, b)
  | eq' a b = markPoint g a
  | otherwise = move ops (markPoint g a) (apply ops a, b)

markPoint :: Grid -> Point -> Grid
markPoint g p = (fst g, snd g V.// [(indexMap (fst g) p, old+1)])
  where
    old = snd g V.! (indexMap (fst g) p)

-- markPoint :: [Cell] -> Point -> [Cell]
-- markPoint ((g, n):gs) p
--   | eq' g p = ((g, n+1)):gs
--   | otherwise = (g, n):(markPoint gs p) 
-- markPoint [] p = (p,1):[]


ex :: [(Point, Point)]
ex = [((0,9), (5,9)), ((8,0), (0,8)), ((9,4), (3,4)), ((2,2), (2,1)), ((7,0), (7,4)), ((6,4), (2,0)), ((0,9), (2,9)), ((3,4), (1,4)), ((0,0), (8,8)), ((5,5), (8,2))]


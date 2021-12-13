import AOC
import Data.List
import Data.List.Split

-- 9a: 436

main = interactl $ f . map (map read . tail . splitOn "")

localEx1 :: [Int] -> [Bool]
localEx1 = g 9
  where
    g :: Int -> [Int] -> [Bool]
    g p (x1:x2:xs) = (x1 < p && x1 < x2) : g x1 (x2:xs)
    g p [x] = [x < p]
    g _ [] = []

f :: [[Int]] -> Int
f l = sum $ map succ $ concat $ filterMask mask l
  where
    os = map localEx1 l
    ts = transpose $ map localEx1 (transpose l)
    z = zip ts os
    mask = map (uncurry (zipWith (&&))) z

filterMask :: [[Bool]] -> [[Int]] -> [[Int]]
-- filterMask (m:ms) (l:ls) = if m then l : filterMask ms ls else filterMask ms ls
filterMask (m:ms) (l:ls) = g m l : filterMask ms ls
  where
    g :: [Bool] -> [Int] -> [Int]
    g (m:ms) (l:ls) = if m then l : g ms ls else g ms ls
    g [] [] = []
    g _ _ = []
filterMask [] [] = []
filterMask _ _ = []



ex :: [[Int]]
ex = [[2,1,9,9,9,4,3,2,1,0], [3,9,8,7,8,9,4,9,2,1], [9,8,5,6,7,8,9,8,9,2], [8,7,6,7,8,9,6,7,8,9], [9,8,9,9,9,6,5,6,7,8]]

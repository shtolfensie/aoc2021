import AOC
import Data.List
import Data.List.Split
import Data.Array
import Control.Monad

-- 9b: 1317792

main = interactl $ f . map (map read . tail . splitOn "")

localEx1 :: [Int] -> [Bool]
localEx1 = g 9
  where
    g :: Int -> [Int] -> [Bool]
    g p (x1:x2:xs) = (x1 < p && x1 < x2) : g x1 (x2:xs)
    g p [x] = [x < p]
    g _ [] = []

localEx2 :: [[Int]] -> [[Bool]]
localEx2 l = mask
  where
    os = map localEx1 l
    ts = transpose $ map localEx1 (transpose l)
    z = zip ts os
    mask = map (uncurry (zipWith (&&))) z

getEx :: (Int, Int) -> [[Int]] -> [((Int, Int), Int)]
getEx bs = filter ((==1) . snd) . assocs . toArr (snd bs) (fst bs) . map (map fromEnum) . localEx2

findBasin :: Array (Int, Int) Int -> Array (Int, Int) Bool -> (Int, Int) -> Array (Int, Int) Bool
findBasin origA a p = arr4
  where
    arr0 = a // [(p, True)]
    arr1 = if snd p > 0     && not (a!xm1) && origA!xm1>origA!p && origA!xm1 /= 9 then findBasin origA arr0 (fst p, snd p - 1) else arr0
    arr2 = if snd p < snd b && not (a!xp1) && origA!xp1>origA!p && origA!xp1 /= 9 then findBasin origA arr1 (fst p, snd p + 1) else arr1
    arr3 = if fst p > 0     && not (a!ym1) && origA!ym1>origA!p && origA!ym1 /= 9 then findBasin origA arr2 (fst p - 1, snd p) else arr2
    arr4 = if fst p < fst b && not (a!yp1) && origA!yp1>origA!p && origA!yp1 /= 9 then findBasin origA arr3 (fst p + 1, snd p) else arr3
    xm1 = (fst p, snd p - 1)
    xp1 = (fst p, snd p + 1)
    ym1 = (fst p-1, snd p)
    yp1 = (fst p+1, snd p)
    b = snd $ bounds a


slope :: [Int] -> [Bool]
slope = g 9
  where
    g :: Int -> [Int] -> [Bool]
    g p (x1:x2:xs) = (x1 < p || x1 < x2) : g x1 (x2:xs)
    g p [x] = [x < p || x < 9]
    g _ [] = []

emptyArray bs =  toArr (snd bs) (fst bs) . map (map (const False))

f :: [[Int]] -> Int
f l = b1 * b2 * b3
  where
    exs = getEx (rs,cs) l
    rs  = length l
    cs  = length $ head l
    a   = emptyArray (rs, cs) l
    la  = toArr cs rs l
    (b1:b2:b3:_) = sortBy (flip compare) $ map (sum . map fromEnum . elems . findBasin la (emptyArray (rs, cs) l) . fst) exs

filterMask :: [[Bool]] -> [[Int]] -> [[Int]]
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

exA = toArr 10 5 ex

print2d :: [[Bool]] -> String
print2d (l:ls) = (++print2d ls) $ (++"\n") $ show $ map fromEnum l
print2d [] = ""

print2d1 :: [[[Bool]]] -> String
print2d1 (l:ls) = (++print2d1 ls) $ (++"\n") $ show $ map (map fromEnum) l
print2d1 [] = ""

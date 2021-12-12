import AOC
import Data.List.Split

-- 6a: 386755

main = interacts "," $ f 0 . map read

f :: Int -> [Int] -> Int
f 80 s = length s
f d  s = f (succ d) (day s)

day :: [Int] -> [Int]
day (fi:fs) = if fi == 0 then 8:(6: day fs) else (fi-1): day fs
day [] = []


import AOC
import Data.List.Split

-- 8a: 318

main = interactl $ sum . map (f . map words . splitOn " | ")
-- $ g . map words

g [_, os] = os
g _ = []

f :: [[String]] -> Int
f [_, os] = length $ filter ((`elem`[2,3,4,7]) . length) os
f _ = 0

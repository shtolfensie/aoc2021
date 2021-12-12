import AOC
import Data.List

main = interacts "," $ f . ints

f is = minimum $ map (`dst` is) u
  where
    sorted = sort is
    mx = maximum sorted
    mn = head sorted
    a = avg sorted
    groups = sortOn snd . (\g -> zip g (map length g)) . group $ sorted
    u = nub is

dst :: Int -> [Int] -> Int
dst t is = sum $ map (abs . (t-)) is

avg :: [Int] -> Int
avg is = sum is `div` length is

-- 2 -> 5 = dst: 3 cost: 6
-- 3:1 4:2 5:3

-- 14 -> 5 = dst: 9 cost: 45
-- 13:1 12:2 11:3 10:4 9:5 8:6 7:7 6:8 5:9

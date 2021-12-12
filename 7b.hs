import AOC
import Data.List

-- 7a: 344605
-- 7b: 93699985

main = interacts "," $ f . ints

f is = minimum $ map (\d -> sum $ map (fuel . dst d) is) [mn..mx]
  where
    sorted = sort is
    mx = maximum sorted
    mn = head sorted
    a = avg sorted
    groups = sortOn snd . (\g -> zip g (map length g)) . group $ sorted
    u = nub is

dst :: Int -> Int -> Int
dst t = abs . (t-)

avg :: [Int] -> Int
avg is = sum is `div` length is

fuel :: Int -> Int
fuel d = round $ (1+ fromIntegral d) / 2 * fromIntegral d

-- 2 -> 5 = dst: 3 cost: 6
-- 3:1 4:2 5:3

-- 1 -> 5 = dst: 4 cost: 10
-- 2:1 3:2 4:3 5:4

-- 14 -> 5 = dst: 9 cost: 45
-- 13:1 12:2 11:3 10:4 9:5 8:6 7:7 6:8 5:9

ex :: [Int]
ex = [16,1,2,0,4,2,7,1,2,14]

import AOC
import Data.List

-- 3a: 3813416

main = interactl f

toDec = foldl (\y x -> fromEnum x + y*2) 0

mostcommon ll = toDec . map ((\l -> ll-l < l) . length . filter (=='1'))

leastcommon ll = toDec . map ((\l -> ll-l > l) . length . filter (=='1'))

f xs = gamma * epsilon
  where
    gamma = mostcommon cl cols
    epsilon = leastcommon cl cols
    cl = length $ head cols
    cols = transpose xs


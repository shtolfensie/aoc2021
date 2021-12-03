import AOC
import Data.List

-- 3a: 3813416

main = interactl f'

both =  map ((\(o,z) -> (length z < length o, length z > length o)) . partition (=='1'))

toDecTuple :: [(Bool, Bool)] -> (Int, Int)
toDecTuple = foldl (\(a,b) (x,y) -> (fromEnum x + a*2, fromEnum y + b*2)) (0,0)

f' xs = uncurry (*) res
  where
    res = toDecTuple $ both $ transpose xs

import AOC
import Data.List
import Data.Bifunctor (Bifunctor(bimap))

-- 3a: 3813416
-- 3b: 2990784

main = interactl f'

both =  map ((\(o,z) -> (length z < length o, length z > length o)) . partition (=='1'))


filterRow :: Bool -> Char -> Bool
filterRow m c
  | head (show (fromEnum m)) == c = True
  | otherwise = False

filterRows :: Int -> Bool -> [String] -> [String]
filterRows _ _  [x] = [x]
filterRows  n o xs = filterRows (n+1) o $ filter (filterRow m . head . drop n) xs
  where
    (m1,m2) = head $ drop n $ both $ transpose xs
    m
      | m1 == m2 = o && True
      | o = m1
      | otherwise = m2




inp = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]

f' xs = multPair $ bimap (toDec' . head) ( toDec' . head) (filterRows 0 True xs, filterRows 0 False xs)
  -- where
    -- mask = both $ transpose xs -- most/least common for each column


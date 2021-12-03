import Prelude
import Data.List.HT

-- | Blackbird combinator
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

f :: [Int] -> Int
f =  sum . mapAdjacent (fromEnum .: (<))





main = interact $ (++"\n") . show . f . map read . lines

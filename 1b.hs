import Prelude
import Data.List.HT
import Data.Tuple.HT

-- | Blackbird combinator
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

window3 xs = zip3 xs (tail xs) (tail $ tail xs) 

sumTriple (x,y,z) = x + y + z

f :: [Int] -> Int
f =  sum . mapAdjacent (fromEnum .: (<)) . map sumTriple . window3





main = interact $ (++"\n") . show . f . map read . lines

import AOC
import Data.List.Split
import qualified Data.Vector as V

-- 6a: 386755
-- 6b: 1732731810807

main = interacts "," $ solve 256 . map read

solve :: Int -> [Int] -> Int
solve d s = (ol +) $ sum $ kids 0 d (f (V.replicate (d+1) 0) d s)
  where
    ol = length s

f :: V.Vector Int -> Int -> [Int] -> V.Vector Int
f ds md s = nfish
  where
    nfish :: V.Vector Int
    nfish = foldr (\c a -> fish c a md) ds s

-- fish :: V.Vector Int -> Int -> Int -> V.Vector Int
fish so ds md = ds V.// zip (rd so 0 md) (map (succ . (ds V.!)) (rd so 0 md))

rd so d md = [so+1+d, (so+1+d+7) .. md]

kids :: Int -> Int -> V.Vector Int -> V.Vector Int
kids 256 _ fd = fd
kids i md fd = kids (i+1) md (fd V.// zip (rd 8 i md) (map (((1*nf)+) . (fd V.!)) (rd 8 i md)))
  where
    nf = fd V.! i



import AOC
import Data.List.Split
import qualified Data.Vector as V
import Text.Parsec.Combinator
import Data.List

-- 8a: 318

--             0   1   2   3   4   5   6
data Segment = A | B | C | D | E | F | G deriving (Show, Bounded, Eq, Enum)

type MixedSegment = [Segment]
type Patterns = [[Segment]]
type Pattern = [Segment]

type Display = V.Vector MixedSegment
type DecodedDisplay = V.Vector (Int, Segment)



-- 1 -> 7  ->  4    >=> 3 (A,B,D,G)-> 0(E)   -> 6 (F,C)
-- oo    AA    oo     AA             AA      AA     
--o  C? o  C? B? C?  o  C?          B  C?   B  o    
-- oo    oo    DD?    DD             oo      DD     
--o  F? o  F? o  F?  o  F?          E  F?   E  F    
-- oo    oo    oo     GG             GG      GG     

main = interactl $ sum . map (f . map words . splitOn " | ")

-- f :: [[String]] -> [[[Segment]]]
-- f [ps, os] = patterns ps
-- f _ = []
f [ps, os] = fromDigits $ map (decodeNumber dd) ops
  where
    dd = decodeDisplay $ decipher pas
    pas = patterns ps
    ops = rights $ map (parse p) os
f _ = -2

decodeDisplay :: Display -> DecodedDisplay
decodeDisplay = V.indexed . V.map head

-- decodeNumber :: DecodedDisplay -> Pattern -> [[(Int, Segment)]]
decodeNumber d p = findDigit $ sort $ map (fst . getSegment d) p

getSegment :: DecodedDisplay -> Segment -> (Int, Segment)
getSegment dd s = V.head $ V.filter (\t -> snd t == s) dd

findDigit :: [Int] -> Int
findDigit p = g 0 p digitMap
  where
    g :: Int -> [Int] -> [[Int]] -> Int
    g i p (m:ms) = if p == m then i else g (i+1) p ms
    g _ _ [] = -1

digitMap :: [[Int]]
digitMap = map (map fromEnum) [[A,B,C,E,F,G], [C,F], [A,C,D,E,G], [A,C,D,F,G], [B,C,D,F], [A,B,D,F,G], [A,B,D,E,F,G], [A,C,F], [A,B,C,D,E,F,G], [A,B,C,D,F,G]]

patterns :: [String] -> [Patterns]
patterns = groupBy (\a b -> length a == length b) . sortOn length . rights . map (parse p)

decipher :: [Patterns] -> Display
decipher pas = six p6elem . zero p6elem . three p5elem . four p4 . seven p7 . one p1 $ V.replicate 7 []
  where
    p1 = head pas
    p7 = head $ tail pas
    p4 = pas !! 2
    p5elem = pas !! 3
    p6elem = pas !! 4

one :: Patterns -> Display -> Display
one p d = d V.// [(fromEnum C, head p), (fromEnum F, head p)]

seven :: Patterns  -> Display -> Display
seven p d = d V.// [(fromEnum A, a)]
  where
    a = filter (not . (`elem`o)) (head p)
    o = d V.! fromEnum C

four :: Patterns  -> Display -> Display
four p d = d V.// [(fromEnum B, bd), (fromEnum D, bd)]
  where
    bd = filter (not . (`elem`o)) (head p)
    o = d V.! fromEnum C

three :: Patterns  -> Display -> Display
three p d = d V.// [(fromEnum D, pd), (fromEnum B, pb), (fromEnum G, pg)]
  where
    p3 = head $ filter (\e -> elem o1 e && elem o2 e) p
    [o1, o2] = d V.! fromEnum C
    pd = filter (`elem`p3) $ d V.! fromEnum D
    pb = filter (not . (`elem`pd)) $ d V.! fromEnum B
    pg = filter (not . (`elem`concat d)) p3

zero :: Patterns  -> Display -> Display
zero p d = d V.// [(fromEnum E, pe)]
  where
    pe = filter (not . (`elem`concat d)) p0
    p0 = head $ filter (notElem pd) p
    [pd] = d V.! fromEnum D

six :: Patterns  -> Display -> Display
six p d = d V.// [(fromEnum F, df), (fromEnum C, dc)]
  where
    p6 = head $ filter (\e -> not (c1 `elem` e && c2 `elem` e)) p
    [c1,c2] = d V.! fromEnum C
    df = filter (`elem`[c1,c2]) p6
    dc = filter (not . (`elem`df)) [c1,c2]



p :: Parser [Segment]
p =
  many enump


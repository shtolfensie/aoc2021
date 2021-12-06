import AOC
import Data.List
import Data.List.Split
import Data.Either

data CellState = Unmarked | Marked deriving (Show, Eq)

type Cell = (CellState, Int)

type Row = [Cell]

type Board = [Row]

f :: [[String]] -> ([Int], [Board])
f (n:bs) = (map read . splitOn "," $head n, boards $ map (map (map read . words)) bs)
f [] = ([], [])

consCell :: Int -> Cell
consCell = (,) Unmarked

consRow :: [Int] -> Row
consRow = map consCell

consBoard :: [[Int]] -> Board
consBoard = map consRow

boards :: [[[Int]]] -> [Board]
boards = map consBoard

markNum :: Int -> Board -> Board
markNum n = map (g n)
  where
    g :: Int -> Row -> Row
    g n (c:cs) = if snd c == n then (Marked, snd c):cs else c:g n cs
    g n [] = []

step :: [Board] -> Int -> [Board]
step bs n = map (markNum n) bs

won :: Board -> Either Int Board
won b = if allMarked b || allMarked b' then Right b else Left 0
  where
    allMarked = any (all ((==Marked). fst))
    b' = transpose b

score :: Board -> Int
score = sum . map (foldl g 0)
  where
    g a (Unmarked, n) = a + n
    g a (Marked, _) = a

game :: ([Int], [Board]) -> Int
game ([], bs) = 69
game (ns, bs) = if not (null winner) then score (head winner) * head ns else game (tail ns, curr)
  where
    curr = step bs $ head ns
    winner :: [Board]
    winner = rights $ map won curr

main = interactl $ game . f . splitOn [""]

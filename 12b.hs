import AOC
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as Map

-- 12b: 89592

main = interactl $ length . f . map (pairs . splitOn "-")

f :: [(String, String)] -> [[String]]
f ps = travelPath [] mp "start"
  where
    unq = nub $ concatMap ll ps
    ll (x,y) = [x,y]
    mp = buildMap unq ps

pairs :: [String] -> (String,String)
pairs [x, y] = (x,y)
pairs _ = ("", "")

travelPath :: [String] -> Map.Map String [String] -> String -> [[String]]
travelPath td mp st
  | null ns = [st:td | st == "end"]
  | st == "end" = [st:td]
  | otherwise = concatMap (travelPath (st:td) mp) ns
  where
    ns = filter (visitCave (st:td)) $ mp Map.! st

visitCave :: [String] -> String -> Bool
visitCave td c
  | c == "start" = False
  | all isUpper c = True
  | otherwise = all ((<=1). length) (group $ sort $ filter (all isLower) td) || (c `notElem` td)

findNeighbors :: String -> [(String, String)] -> [String]
findNeighbors c ((a,b):ps)
  | c == a = b : findNeighbors c ps
  | c == b = a : findNeighbors c ps
  | otherwise = findNeighbors c ps
findNeighbors c [] = []

buildMap :: [String] -> [(String, String)] -> Map.Map String [String]
buildMap unq ps = foldl (\m e -> Map.insert e (findNeighbors e ps) m) Map.empty unq

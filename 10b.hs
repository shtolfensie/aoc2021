import AOC
import Data.Maybe
import Data.List


main = interactl f

openingParens = ['(', '[', '{', '<']

closingParens = [')', ']', '}', '>']

getPair:: Char -> (Char, Char)
getPair x = g openingParens closingParens x
  where
    g (o:os) (c:cs) x
      | x == c = (o, c)
      | x == o = (o, c)
      | otherwise = g os cs x
    g _ _ _ = ('x', 'x')

score :: Char -> Int
score c = findInMap c mp
  where
    findInMap :: Char -> [(Char, Int)] -> Int
    findInMap c ((p, s):ms)
      | c == p = s
      | otherwise = findInMap c ms
    findInMap _ [] = 0
    mp :: [(Char, Int)]
    mp = [(')', 1), (']', 2), ('}', 3), ('>', 4)]


-- f :: [String] -> Int
f = mid . sort . filter (>0) . map (calcScore . catMaybes . wrongParen [])

mid :: [a] -> a
mid l = l !! (length l `div` 2)

calcScore :: [Char] -> Int
calcScore = foldl (\a c -> (a*5) + score c) 0


wrongParen :: [Char] -> String -> [Maybe Char]
wrongParen (p:ps) (c:cs)
  | c `elem` openingParens = wrongParen (c:(p:ps)) cs
  | c `elem` closingParens = if fst (getPair c) == p then wrongParen ps cs else [Nothing]
  | otherwise = [Nothing]
wrongParen [] (c:cs)
  | c `elem` openingParens = wrongParen [c] cs
  | otherwise = [Nothing]
wrongParen (p:ps) [] =  map (\x -> Just (snd $ getPair x)) (p:ps) -- Just 'i'
wrongParen [] [] = [Nothing]












ex :: [String]
ex = ["[({(<(())[]>[[{[]{<()<>>", "[(()[<>])]({[<{<<[]>>(", "{([(<{}[<>[]}>{[]{[(<()>", "(((({<>}<{<{<>}{[]{[]{}", "[[<[([]))<([[{}[[()]]]", "[{[{({}]{}}([{[{{{}}([]", "{<[[]]>}<{[{[{[]{()[[[]", "[<(<(<(<{}))><([]([]()", "<{([([[(<>()){}]>(<<{{", "<{([{{}}[<[[[<>{}]]]>[]]"]

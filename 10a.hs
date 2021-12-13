import AOC
import Data.Maybe

-- 10a: 389589

main = interactl f

openingParens = ['(', '[', '{', '<']

closingParens = [')', ']', '}', '>']

getOpening :: Char -> Char
getOpening x = g openingParens closingParens x
  where
    g (o:os) (c:cs) x
      | x == c = o
      | otherwise = g os cs x
    g _ _ _ = 'x'

score :: Char -> Int
score c = findInMap c mp
  where
    findInMap :: Char -> [(Char, Int)] -> Int
    findInMap c ((p, s):ms)
      | c == p = s
      | otherwise = findInMap c ms
    findInMap _ [] = 0
    mp :: [(Char, Int)]
    mp = [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]


f :: [String] -> Int
f = sum . map score . mapMaybe (wrongParen [])


wrongParen :: [Char] -> String -> Maybe Char
wrongParen (p:ps) (c:cs)
  | c `elem` openingParens = wrongParen (c:(p:ps)) cs
  | c `elem` closingParens = if getOpening c == p then wrongParen ps cs else Just c
  | otherwise = Just 'x'
wrongParen [] (c:cs)
  | c `elem` openingParens = wrongParen [c] cs
  | otherwise = Just c
wrongParen (p:ps) [] = Nothing -- Just 'i'
wrongParen [] [] = Nothing












ex :: [String]
ex = ["[({(<(())[]>[[{[]{<()<>>", "[(()[<>])]({[<{<<[]>>(", "{([(<{}[<>[]}>{[]{[(<()>", "(((({<>}<{<{<>}{[]{[]{}", "[[<[([]))<([[{}[[()]]]", "[{[{({}]{}}([{[{{{}}([]", "{<[[]]>}<{[{[{[]{()[[[]", "[<(<(<(<{}))><([]([]()", "<{([([[(<>()){}]>(<<{{", "<{([{{}}[<[[[<>{}]]]>[]]"]

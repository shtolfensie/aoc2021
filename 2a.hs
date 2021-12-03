{-# LANGUAGE ScopedTypeVariables #-}
import AOC
import Data.Char

-- 2a: 2102357
-- 2b: 2101031224

data Dir = Forward | Down | Up deriving (Show, Bounded, Eq, Enum)

main :: IO ()
main = interactl $ f . rights . map (parse p)

chari :: Char -> Parser Char
chari c = oneOf [toLower c, toUpper c]

stringi :: String -> Parser String
stringi = mapM chari

enump :: forall b. (Enum b, Bounded b, Show b) => Parser b
enump = choice $ map sr [minBound :: b..maxBound :: b]
  where
    sr :: (Show b) => b -> Parser b
    sr x = try $ stringi (show x) >> return x


rights (Right x:xs) = x: rights xs
rights (_:xs) = rights xs
rights [] = []


p :: Parser (Dir, Int)
p = do
  dirStr <- enump
  char ' '
  amount <- many1 digit
  return (dirStr, read amount)

f :: [(Dir, Int)] -> Int
f xs = uncurry (*) $ foldl c (0,0) xs
  where
    c :: (Int, Int) -> (Dir, Int) -> (Int, Int)
    c acc coord
      | dir == Forward = (h+a, d)
      | dir == Down = (h, d+a)
      | dir == Up = (h, d-a)
      | otherwise = acc
      where
        dir = fst coord
        a = snd coord
        h = fst acc
        d = snd acc


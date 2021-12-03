{-# LANGUAGE ScopedTypeVariables #-}
import AOC hiding(Down)
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

move (x, y) (Forward, n) = (x+n, y)
move (x, y) (Down, n) = (x, y+n)
move (x, y) (Up, n) = (x, y-n)

f :: [(Dir, Int)] -> Int
f xs = x * y
  where
    (x, y) = foldl move (0,0) xs


{-# LANGUAGE ScopedTypeVariables #-}
import AOC hiding(Down)
import Data.Char
import Data.Tuple.HT


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

move (x, y, a) (Forward, n) = (x+n, y+(a*n), a)
move (x, y, a) (Down, n) = (x, y, a+n)
move (x, y, a) (Up, n) = (x, y, a-n)

f :: [(Dir, Int)] -> Int
f xs = x * y
  where
    (x, y, _) = foldl move (0,0,0) xs


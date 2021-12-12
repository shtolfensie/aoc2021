{-# LANGUAGE ScopedTypeVariables #-}
module AOC(module Prelude, module AOC, module Text.Parsec) where

import Prelude hiding(interact)
import qualified Prelude

import Text.Parsec hiding(count, parse)
import qualified Text.Parsec as Parsec

import qualified Data.List.Split as Split

import Data.Char

interactw :: Show a => ([String] -> a) -> IO()
interactw f = Prelude.interact $ (++"\n") . show . f . words

interacts :: Show a => String -> ([String] -> a) -> IO()
interacts p f = Prelude.interact $ (++"\n") . show . f . Split.splitOn p

interactl :: Show a => ([String] -> a) -> IO ()
interactl f = Prelude.interact $ (++"\n") . show . f . lines


rights (Right x:xs) = x: rights xs
rights (_:xs) = rights xs
rights [] = []

type Parser = Parsec String ()

parse :: Parser a -> String -> Either ParseError a
parse p = Parsec.parse p ""

chari :: Char -> Parser Char
chari c = oneOf [toLower c, toUpper c]

stringi :: String -> Parser String
stringi = mapM chari

enump :: forall b. (Enum b, Bounded b, Show b) => Parser b
enump = choice $ map sr [minBound :: b..maxBound :: b]
  where
    sr :: (Show b) => b -> Parser b
    sr x = try $ stringi (show x) >> return x

fromDigits :: [Int] -> Int
fromDigits = foldl (\y x -> x + y*10) 0

toDec :: [Bool] -> Int
toDec = foldl (\y x -> fromEnum x + y*2) 0

toDec' :: [Char] -> Int
toDec' = foldl (\y x -> fromEnum x + y*2) 0 . map (=='1')

multPair :: (Int, Int) -> Int
multPair = uncurry (*)

ints :: [String] -> [Int]
ints = map read

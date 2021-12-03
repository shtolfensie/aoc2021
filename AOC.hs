module AOC(module Prelude, module AOC, module Text.Parsec) where

import Prelude hiding(interact)
import qualified Prelude

import Text.Parsec hiding(count, parse)
import qualified Text.Parsec as Parsec

interactl :: Show a => ([String] -> a) -> IO ()
interactl f = Prelude.interact $ (++"\n") . show . f . lines

type Parser = Parsec String ()

parse :: Parser a -> String -> Either ParseError a
parse p = Parsec.parse p ""

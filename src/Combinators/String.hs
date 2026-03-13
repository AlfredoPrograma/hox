module Combinators.String where

import Combinators.Char (satisfy)
import Combinators.Parser (Parser (..))
import Combinators.Repetition (many1)

-- Takes characters while predicate is truthty and returns the built string.
while :: (Char -> Bool) -> Parser String
while f = many1 $ satisfy f

-- Takes character until predicate is truthty and returns the built string.
until :: (Char -> Bool) -> Parser String
until f = many1 $ satisfy (not . f)

-- Gets the element within the open and close parsers
bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do
  _ <- open
  x <- p
  _ <- close
  return x

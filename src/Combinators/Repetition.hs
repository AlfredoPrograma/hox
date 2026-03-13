module Combinators.Repetition where

import Combinators.Parser (Parser (..))
import Control.Applicative (Alternative ((<|>)))

-- Runs parser one or more times and accumulates the result into a list.
many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  acc <- many1 p <|> pure []
  return (x : acc)

-- Runs parser zero or more times and accumulates the result into a list.
many0 :: Parser a -> Parser [a]
many0 p = many1 p <|> pure []

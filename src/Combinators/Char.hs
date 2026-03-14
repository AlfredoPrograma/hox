module Combinators.Char where

import Combinators.Parser (Parser (..))
import Control.Monad (void)

-- Applies predicate to the next character of the source and consumes it only if
-- predicate is True.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser validate
  where
    validate (x : xs)
      | f x = Just (x, xs)
      | otherwise = Nothing
    validate [] = Nothing

-- Matches next character of the source with the target character.
char :: Char -> Parser Char
char ch = satisfy (== ch)

-- Consumes next character and ignores it.
next :: Parser ()
next = void $ satisfy (const True)

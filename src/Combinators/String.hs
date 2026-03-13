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

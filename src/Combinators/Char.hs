{-# LANGUAGE LambdaCase #-}

module Combinators.Char where

import Combinators.Parser (ParseState (..), Parser (..))
import Combinators.Repetition (many1)

-- Matches next character of the source with the target character.
char :: Char -> Parser Char
char ch = Parser validate
  where
    validate s@ParseState {source = (x : xs)}
      | x == ch = Just (x, s {source = xs})
      | otherwise = Nothing
    validate ParseState {source = []} = Nothing

-- Applies predicate to next character of the source and success if True.
charPred :: (Char -> Bool) -> Parser Char
charPred f = Parser validate
  where
    validate s@ParseState {source = (x : xs)}
      | f x = Just (x, s {source = xs})
      | otherwise = Nothing
    validate ParseState {source = []} = Nothing

-- Applies predicate to the next character of the source and consumes it only if
-- predicate is True.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $
  \case
    s@ParseState {source = (x : xs)} ->
      if f x
        then Just (x, s {source = xs})
        else Nothing
    ParseState {source = []} -> Nothing

-- Takes characters while predicate is truthty and returns the built string.
while :: (Char -> Bool) -> Parser String
while f = many1 $ satisfy f

-- Takes character until predicate is truthty and returns the built string.
until :: (Char -> Bool) -> Parser String
until f = many1 $ satisfy (not . f)

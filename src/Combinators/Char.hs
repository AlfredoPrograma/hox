{-# LANGUAGE LambdaCase #-}

module Combinators.Char where

import Combinators.Parser (ParseState (..), Parser (..))

-- Matches next character of the source with the target character.
char :: Char -> Parser Char
char ch = Parser validate
  where
    validate s@ParseState {source = (x : xs)}
      | x == ch = Just (x, s {source = xs})
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

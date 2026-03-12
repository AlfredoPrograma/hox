module Combinators.Parser where

import Data.Bifunctor (first)

data ParseState = ParseState
  { rest :: String, -- Rest of the input to be consumed.
    line :: Int -- Current line of the parsing process position.
  }

-- A `Parser` wraps a function from current parsing state to deterministic result and resulting parsing state.
newtype Parser a = Parser {parse :: ParseState -> Maybe (a, ParseState)}

instance Functor Parser where
  fmap f p =
    Parser $
      \state -> first f `fmap` parse p state

instance Applicative Parser where
  pure x = Parser $
    \state -> Just (x, state)

  p <*> q = Parser $
    \state -> do
      (f, r) <- parse p state
      (x, r') <- parse q r
      pure (f x, r')

instance Monad Parser where
  p >>= f = Parser $
    \state -> do
      (x, r) <- parse p state
      parse (f x) r

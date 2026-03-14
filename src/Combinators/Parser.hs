module Combinators.Parser where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Bifunctor (first)

-- A `Parser` wraps a function from current parsing state to deterministic result and resulting parsing state.
newtype Parser a = Parser {parse :: String -> Maybe (a, String)}

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

instance Alternative Parser where
  empty = Parser $ const Nothing

  p <|> q = Parser $
    \state -> parse p state <|> parse q state

instance Monad Parser where
  p >>= f = Parser $
    \state -> do
      (x, r) <- parse p state
      parse (f x) r

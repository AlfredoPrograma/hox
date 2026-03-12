module Combinators.Parser where

data ParseState = ParseState
  { rest :: String, -- Rest of the input to be consumed
    line :: Int -- Current line of the parsing process position
  }

newtype Parser a = Parser {parse :: String -> (Maybe a, ParseState)}

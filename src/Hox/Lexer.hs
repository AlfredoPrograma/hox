{-# LANGUAGE NamedFieldPuns #-}

module Hox.Lexer where

import Combinators.Char (char)
import Combinators.Parser (Parser (..))
import Combinators.Repetition (many1)
import Control.Applicative (Alternative ((<|>)))

data TokenKind
  = LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  deriving (Show, Eq)

data Token = Token
  { kind :: TokenKind,
    lexeme :: String
  }
  deriving (Show, Eq)

scanTokens :: Parser [Token]
scanTokens = many1 singleCharToken

singleCharToken :: Parser Token
singleCharToken =
  asToken '(' LeftParen
    <|> asToken ')' RightParen
    <|> asToken '{' LeftBrace
    <|> asToken '}' RightBrace
    <|> asToken ',' Comma
    <|> asToken '.' Dot
    <|> asToken '-' Minus
    <|> asToken '+' Plus
    <|> asToken ';' Semicolon
    <|> asToken '/' Slash
    <|> asToken '*' Star
  where
    asToken ch kind = do
      _ <- char ch
      return $
        Token
          { kind,
            lexeme = [ch]
          }

module Hox.Lexer where

import Combinators.Char (char)
import Combinators.Parser (Parser (..))
import Combinators.Repetition (many1)
import Control.Applicative (Alternative ((<|>)))

data TokenKind
  = -- Single char tokens
    LeftParen
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
  | -- Single but also two char chainable tokens
    Bang
  | BangEq
  | Eq
  | DoubleEq
  | Greater
  | GreaterEq
  | Less
  | LessEq
  deriving (Show, Eq)

data Token = Token
  { kind :: TokenKind,
    lexeme :: String
  }
  deriving (Show, Eq)

scanTokens :: Parser [Token]
scanTokens = many1 singleCharToken

twoCharChainableToken :: Parser Token
twoCharChainableToken = fmap toToken (pair <|> single)
  where
    -- TODO: careful with backtracking. prefix (single) is being computed per each parsing possibility
    toToken str = Token {kind = fromLexeme str, lexeme = str}
    single = do
      ch <- char '!' <|> char '=' <|> char '<' <|> char '>'
      return [ch]
    pair = do
      pre <- single
      eq <- char '='
      return $ reverse (eq : pre)

singleCharToken :: Parser Token
singleCharToken =
  charAsToken '('
    <|> charAsToken ')'
    <|> charAsToken '{'
    <|> charAsToken '}'
    <|> charAsToken ','
    <|> charAsToken '.'
    <|> charAsToken '-'
    <|> charAsToken '+'
    <|> charAsToken ';'
    <|> charAsToken '/'
    <|> charAsToken '*'

charAsToken :: Char -> Parser Token
charAsToken ch = do
  str <- fmap (: []) (char ch)
  return $
    Token {kind = fromLexeme str, lexeme = str}

fromLexeme :: String -> TokenKind
fromLexeme str = case str of
  [] -> error "invalid empty string"
  "(" -> LeftParen
  ")" -> RightParen
  "{" -> LeftBrace
  "}" -> RightBrace
  "," -> Comma
  "." -> Dot
  "-" -> Minus
  "+" -> Plus
  ";" -> Semicolon
  "/" -> Slash
  "*" -> Star
  "!" -> Bang
  "=" -> Eq
  "<" -> Less
  ">" -> Greater
  "!=" -> BangEq
  "==" -> DoubleEq
  "<=" -> LessEq
  ">=" -> GreaterEq
  _ -> error "unrecognized lexeme"

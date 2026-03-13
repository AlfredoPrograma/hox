{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hox.Lexer where

import Combinators.Char (char, satisfy)
import Combinators.Parser (ParseState (..), Parser (..))
import Combinators.Repetition (many0, many1)
import Combinators.String (bracket, until)
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (void)

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
  | -- Multichar tokens
    Str
  deriving (Show, Eq)

data Token = Token
  { kind :: TokenKind,
    lexeme :: String
  }
  deriving (Show, Eq)

scanTokens :: Parser [Token]
scanTokens = do
  _ <- whitespaces
  _ <- newlines
  _ <- comment
  many1
    ( string
        <|> twoCharChainableToken
        <|> singleCharToken
    )

string :: Parser Token
string = bracket (char '"') token (char '"')
  where
    -- TODO: maybe improve and add some "invalidChars" binding and fold it into a single boolean
    -- TODO: add multiline support and line parse state updating on them
    content = many0 $ satisfy (\ch -> ch /= '\n' && ch /= '"')
    token = fmap (Token Str) content

newlines :: Parser ()
newlines = void $ many0 newline
  where
    newline = Parser $
      \case
        ParseState {source = []} -> Nothing
        s@ParseState {line, source = (_ : xs)} -> do
          _ <- parse (char '\n') s
          return ((), ParseState {line = line + 1, source = xs})

whitespaces :: Parser ()
whitespaces = void (many0 ws)
  where
    ws = char ' ' <|> char '\r' <|> char '\t'

comment :: Parser ()
comment = comment' <|> pure ()
  where
    comment' = do
      _ <- char '/'
      _ <- char '/'
      _ <- Combinators.String.until (== '\n')
      return ()

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

-- Helpers

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

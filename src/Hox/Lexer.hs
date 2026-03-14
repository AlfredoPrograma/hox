module Hox.Lexer where

import Combinators.Char (char, satisfy)
import Combinators.Parser (Parser (..))
import Combinators.Repetition (many0, many1)
import Combinators.String (bracket)
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (void)
import Data.Char (isAlphaNum, isSpace)

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
  | Number
  | Identifier
  | -- Keywords
    And
  | Classy
  | Else
  | Falsy
  | For
  | Fun
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | Truthty
  | Var
  | While
  deriving (Show, Eq)

data Token = Token
  { kind :: TokenKind,
    lexeme :: String
  }
  deriving (Show, Eq)

scanTokens :: Parser [Token]
scanTokens = many1 scanToken
  where
    scanToken = do
      _ <- whitespaces
      keywordOrIdentifier
        <|> string
        <|> number
        <|> twoCharChainableToken
        <|> singleCharToken

keywordOrIdentifier :: Parser Token
keywordOrIdentifier = kwOrId
  where
    keywords = ["and", "class", "else", "false", "for", "fun", "if", "nil", "or", "print", "return", "super", "this", "true", "var", "while"]
    validChars = many1 $ satisfy validChar
    validChar ch = isAlphaNum ch || ch == '_'
    kwOrId = do
      chs <- validChars
      if chs `elem` keywords
        then return $ Token (fromLexeme chs) chs
        else return $ Token Identifier chs

number :: Parser Token
number = Token Number <$> rawNumber
  where
    rawNumber = float <|> int <|> ((: []) <$> digitOrZero)
    digit = satisfy (\ch -> ch >= '1' && ch <= '9')
    digitOrZero = digit <|> char '0'
    int = do
      d <- digit
      rest <- many0 digitOrZero
      return (d : rest)
    float = do
      i <- int
      _ <- char '.'
      f <- many1 digitOrZero
      return (i ++ "." ++ f)

string :: Parser Token
string = bracket (char '"') token (char '"')
  where
    -- TODO: maybe improve and add some "invalidChars" binding and fold it into a single boolean
    -- TODO: add multiline support and line parse state updating on them
    content = many0 $ satisfy (\ch -> ch /= '\n' && ch /= '"')
    token = Token Str <$> content

whitespaces :: Parser ()
whitespaces = void $ many0 (satisfy isSpace)

twoCharChainableToken :: Parser Token
twoCharChainableToken = toToken <$> (pair <|> single)
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
  str <- (: []) <$> char ch
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
  "and" -> And
  "class" -> Classy
  "else" -> Else
  "false" -> Falsy
  "for" -> For
  "fun" -> Fun
  "if" -> If
  "nil" -> Nil
  "or" -> Or
  "print" -> Print
  "return" -> Return
  "super" -> Super
  "this" -> This
  "true" -> Truthty
  "var" -> Var
  "while" -> While
  _ -> error "unrecognized lexeme"

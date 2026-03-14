module Main where

import Combinators.Parser (Parser (parse))
import Hox.Lexer (scanTokens)

main :: IO ()
main = do
  content <- readFile "sample.hox"
  case parse scanTokens content of
    Nothing -> putStrLn "Lexing failed"
    Just (tokens, _) -> print tokens

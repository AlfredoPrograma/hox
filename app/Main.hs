module Main where

import Hox.Lexer (lexHox)

main :: IO ()
main = do
  putStrLn lexHox

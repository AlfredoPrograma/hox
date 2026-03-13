module Hox.LexerSpec (spec) where

import Combinators.Parser (ParseState (..), Parser (parse))
import Combinators.Repetition (many1)
import Data.Maybe (fromJust, isNothing)
import Hox.Lexer (Token (..), TokenKind (..), singleCharToken)
import Test.Hspec

spec :: Spec
spec = do
  describe "parses single char token" $ do
    it "return token for each valid single char lexeme" $ do
      let input = ParseState "(){},.-+;/*" 1
          result = fst $ fromJust $ parse (many1 singleCharToken) input
          expected =
            [ Token LeftParen "(",
              Token RightParen ")",
              Token LeftBrace "{",
              Token RightBrace "}",
              Token Comma ",",
              Token Dot ".",
              Token Minus "-",
              Token Plus "+",
              Token Semicolon ";",
              Token Slash "/",
              Token Star "*"
            ]
      result `shouldBe` expected

    it "fail for invalid single char lexemes" $ do
      let input = ParseState "@" 1
          result = parse singleCharToken input
      isNothing result `shouldBe` True

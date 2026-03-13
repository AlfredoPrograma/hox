module Hox.LexerSpec (spec) where

import Combinators.Parser (ParseState (..), Parser (parse))
import Combinators.Repetition (many1)
import Data.Maybe (fromJust, isNothing)
import Hox.Lexer (Token (..), TokenKind (..), comment, singleCharToken, twoCharChainableToken)
import Test.Hspec

spec :: Spec
spec = do
  describe "parses single char token" $ do
    it "returns token for each valid single char lexeme" $ do
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

    it "fails for invalid single char lexemes" $ do
      let input = ParseState "@" 1
          result = parse singleCharToken input
      isNothing result `shouldBe` True

  describe "parses single but also two char chainable lexemes" $ do
    it "returns token for non chained lexemes" $ do
      let input = ParseState "=!<>" 1
          result = fst $ fromJust $ parse (many1 twoCharChainableToken) input
          expected =
            [ Token Eq "=",
              Token Bang "!",
              Token Less "<",
              Token Greater ">"
            ]
      result `shouldBe` expected

    it "returns token for two chained char lexemes" $ do
      let input = ParseState "==!=<=>=" 1
          result = fst $ fromJust $ parse (many1 twoCharChainableToken) input
          expected =
            [ Token DoubleEq "==",
              Token BangEq "!=",
              Token LessEq "<=",
              Token GreaterEq ">="
            ]
      result `shouldBe` expected

    it "fails for invalid chainable char lexemes" $ do
      let input = ParseState "@" 1
          result = parse twoCharChainableToken input
      isNothing result `shouldBe` True

  describe "ignores comments" $ do
    it "consumes input but ignores comments" $ do
      let input = ParseState "// this is a comment\n" 1
          result = snd $ fromJust $ parse comment input
          expected = ParseState "\n" 1
      source result `shouldBe` source expected

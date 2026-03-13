module Hox.LexerSpec (spec) where

import Combinators.Parser (ParseState (..), Parser (parse))
import Combinators.Repetition (many1)
import Data.Maybe (fromJust, isNothing)
import Hox.Lexer (Token (..), TokenKind (..), comment, newlines, singleCharToken, twoCharChainableToken, whitespaces)
import Test.Hspec

spec :: Spec
spec = do
  describe "single char lexemes" $ do
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

  describe "two char chainable lexemes" $ do
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

  describe "comments" $ do
    it "consumes input but ignores comments" $ do
      let input = ParseState "// this is a comment\n" 1
          result = source $ snd $ fromJust $ parse comment input
          expected = source $ ParseState "\n" 1
      result `shouldBe` expected

  describe "whitespaces" $ do
    it "consumes input but ignores whitespaces" $ do
      let input = ParseState " \t \r \n" 1
          result = source $ snd $ fromJust $ parse whitespaces input
          expected = source $ ParseState "\n" 1
      result `shouldBe` expected

  describe "newlines" $ do
    it "consumes input but ignores newline and also updates state" $ do
      let input = ParseState "\n\n\n" 1
          result = source $ snd $ fromJust $ parse newlines input
          expected = source $ ParseState "" 3
      result `shouldBe` expected

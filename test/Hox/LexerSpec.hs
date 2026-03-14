module Hox.LexerSpec (spec) where

import Combinators.Parser (Parser (parse))
import Combinators.Repetition (many1)
import Data.Maybe (fromJust, isNothing)
import Hox.Lexer (Token (..), TokenKind (..), keywordOrIdentifier, number, singleCharToken, string, twoCharChainableToken, whitespaces)
import Test.Hspec

spec :: Spec
spec = do
  describe "single char lexemes" $ do
    it "returns token for each valid single char lexeme" $ do
      let input = "(){},.-+;/*"
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
      let input = "@"
          result = parse singleCharToken input
      isNothing result `shouldBe` True

  describe "two char chainable lexemes" $ do
    it "returns token for non chained lexemes" $ do
      let input = "=!<>"
          result = fst $ fromJust $ parse (many1 twoCharChainableToken) input
          expected =
            [ Token Eq "=",
              Token Bang "!",
              Token Less "<",
              Token Greater ">"
            ]
      result `shouldBe` expected

    it "returns token for two chained char lexemes" $ do
      let input = "==!=<=>="
          result = fst $ fromJust $ parse (many1 twoCharChainableToken) input
          expected =
            [ Token DoubleEq "==",
              Token BangEq "!=",
              Token LessEq "<=",
              Token GreaterEq ">="
            ]
      result `shouldBe` expected

    it "fails for invalid chainable char lexemes" $ do
      let input = "@"
          result = parse twoCharChainableToken input
      isNothing result `shouldBe` True

  describe "keywordOrIdentifier" $ do
    it "returns token for identifier" $ do
      let input = "myVar"
          result = fst $ fromJust $ parse keywordOrIdentifier input
          expected = Token Identifier "myVar"
      result `shouldBe` expected

    it "returns token for keyword" $ do
      let input = "or"
          result = fst $ fromJust $ parse keywordOrIdentifier input
          expected = Token Or "or"
      result `shouldBe` expected

  describe "string" $ do
    it "returns token for valid input" $ do
      let input = "\"Hello world\""
          result = fst $ fromJust $ parse string input
          expected = Token Str "Hello world"
      result `shouldBe` expected

  describe "number" $ do
    it "returns token for single digit number" $ do
      let input = "1"
          result = fst $ fromJust $ parse number input
          expected = Token Number "1"
      result `shouldBe` expected

    it "returns token for multiple digit integer number" $ do
      let input = "255"
          result = fst $ fromJust $ parse number input
          expected = Token Number "255"
      result `shouldBe` expected

    it "returns token for multiple digit floating point number" $ do
      let input = "120.0592"
          result = fst $ fromJust $ parse number input
          expected = Token Number "120.0592"
      result `shouldBe` expected

    it "returns just integer part for incomplete floating point number" $ do
      let input = "100."
          result = fst $ fromJust $ parse number input
          expected = Token Number "100"
      result `shouldBe` expected

  describe "whitespaces" $ do
    it "consumes input but ignores whitespaces" $ do
      let input = " \t \r \n"
          result = snd $ fromJust $ parse whitespaces input
          expected = ""
      result `shouldBe` expected

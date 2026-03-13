module Main (main) where

import qualified Hox.LexerSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  Hox.LexerSpec.spec

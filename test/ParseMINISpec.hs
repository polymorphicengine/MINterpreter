module ParseMINISpec where

import ParseMINI
import Test.Hspec
import Text.Parsec(parse)
import Text.Parsec.Error

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "whitespace" $ do
    it "always suceeds" $ do
        (parse whitespace "" "\t  foo bar\n") `shouldBe` (Right ())
        (parse whitespace "" "foo bar\n") `shouldBe` (Right ())
  describe "number parser" $ do
    it "parses numbers, fails otherwise" $ do
        (parse numE "" "123") `shouldBe` (Right (ENum 123))
        show (parse numE "" "abc") `shouldBe` "Left (line 1, column 1):\nunexpected \"a\"\nexpecting digit"

{-# LANGUAGE QuasiQuotes #-}
module Data.String.InterpolateSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Data.Char

import           Data.String.Interpolate

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "[i|...|]" $ do
    it "interpolates an expression of type Int" $ do
      property $ \x y -> [i|foo #{x + y :: Int} bar|] `shouldBe` "foo " ++ show (x + y) ++ " bar"

    it "interpolates an expression of type String" $ do
      property $ \xs ys -> [i|foo #{xs ++ ys} bar|] `shouldBe` "foo " ++ xs ++ ys ++ " bar"

    it "accepts character escapes" $ do
      [i|foo \955 bar|] `shouldBe` "foo \955 bar"

    it "accepts character escapes in interpolated expressions" $ do
      [i|foo #{"\955" :: String} bar|] `shouldBe` "foo \955 bar"

    it "dose not strip backslashes (issue #1)" $ do
      [i|foo\\bar|] `shouldBe` "foo\\bar"

    it "allows to prevent interpolation by escaping the hash with a backslash" $ do
      [i|foo \#{23 :: Int} bar|] `shouldBe` "foo #{23 :: Int} bar"

    it "does not prevent interpolation on literal backslash" $ do
      [i|foo \\#{23 :: Int} bar|] `shouldBe` "foo \\23 bar"

  describe "normalizeLines" $ do
    it "is total" $ do
      property $ \ string -> length (normalizeLines string) >= 0

    it "removes indentation consistently" $ do
      normalizeLines [i|
        foo
          bar
            foo

        baz
       |] `shouldBe`
        "foo\n  bar\n    foo\n\nbaz\n"

    let headMay (a : _) = Just a
        headMay [] = Nothing
    it "removes the first line if it only consists of whitespace" $ do
      property $ \ text ->
        (maybe False (any (not . isSpace)) (headMay (lines text))) ==>
        (\ n ->
          normalizeLines (replicate n ' ' ++ "\n" ++ text) ===
            normalizeLines text)

    it "disregards lines containing only whitespace when calculating indentation" $ do
      normalizeLines "    foo\n  \n      \n    bar" `shouldBe`
        "foo\n\n  \nbar"

    it "allows to create strings with no trailing newline" $ do
      normalizeLines [i|
        foo|] `shouldBe`
          "foo"

    it "does not end with a newline when the input ends with something other than a whitespace or newline" $ do
      property $ \ string ->
        not (null string) ==>
        not (last string `elem` [' ', '\n']) ==>
        let result = normalizeLines string
        in not (null result) ==>
           last result /= '\n'

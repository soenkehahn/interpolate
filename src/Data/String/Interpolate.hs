{-# LANGUAGE TemplateHaskell #-}
module Data.String.Interpolate (
-- * String interpolation done right
-- |
-- The examples in this module use `QuasiQuotes`.  Make sure to enable the
-- corresponding language extension.
--
-- >>> :set -XQuasiQuotes
-- >>> import Data.String.Interpolate
  i
, unindent
) where

import           Control.Arrow ((>>>))
import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           Language.Haskell.Meta.Parse.Careful (parseExp)
import           Data.Char

import           Data.String.Interpolate.Util
import           Data.String.Interpolate.Parse
import           Data.String.Interpolate.Compat (Q, Exp, appE, reportError)

-- |
-- A `QuasiQuoter` for string interpolation.  Expression enclosed within
-- @#{...}@ are interpolated, the result has to be in the `Show` class.
--
-- It interpolates strings
--
-- >>> let name = "Marvin"
-- >>> putStrLn [i|name: #{name}|]
-- name: Marvin
--
-- or integers
--
-- >>> let age = 23
-- >>> putStrLn [i|age: #{age}|]
-- age: 23
--
-- or arbitrary Haskell expressions
--
-- >>> let profession = "\955-scientist"
-- >>> putStrLn [i|profession: #{unwords [name, "the", profession]}|]
-- profession: Marvin the λ-scientist
i :: QuasiQuoter
i = QuasiQuoter {
    quoteExp = toExp . parseNodes
  , quotePat = err "pattern"
  , quoteType = err "type"
  , quoteDec = err "declaration"
  }
  where
    err name = error ("Data.String.Interpolate.i: This QuasiQuoter can not be used as a " ++ name ++ "!")

    toExp:: [Node] -> Q Exp
    toExp nodes = case nodes of
      [] -> [|""|]
      (x:xs) -> f x `appE` toExp xs
      where
        f (Literal s) = [|showString s|]
        f (Expression e) = [|(showString . toString) $(reifyExpression e)|]

        reifyExpression :: String -> Q Exp
        reifyExpression s = case parseExp s of
          Left _ -> do
            reportError "Parse error in expression!"
            [|""|]
          Right e -> return e

-- | Remove indentation as much as possible while preserving relative
-- indentation levels.
--
-- `unindent` is meant to be used in combination with `i` to allow you to indent
-- your code without affecting your string literals.  Here is an example:
--
-- >>> :{
--  putStr $ unindent [i|
--      def foo
--        23
--      end
--    |]
-- :}
-- def foo
--   23
-- end
--
-- To allow this, two additional things are being done, apart from removing
-- indentation:
--
-- - One empty line at the beginning will be removed and
-- - if the last line consists only of whitespace, it will be trimmed to @"\\n"@.
unindent :: String -> String
unindent input =
    (lines_ >>>
     removeLeadingEmptyLine >>>
     trimLastLine >>>
     removeIndentation >>>
     concat) input
  where
    isEmptyLine :: String -> Bool
    isEmptyLine = all isSpace

    lines_ :: String -> [String]
    lines_ [] = []
    lines_ s = case span (/= '\n') s of
      (first, '\n' : rest) -> (first ++ "\n") : lines_ rest
      (first, rest) -> first : lines_ rest

    removeLeadingEmptyLine :: [String] -> [String]
    removeLeadingEmptyLine ("\n" : r) = r
    removeLeadingEmptyLine l = l

    trimLastLine :: [String] -> [String]
    trimLastLine (a : b : r) = a : trimLastLine (b : r)
    trimLastLine [a] = if all (== ' ') a
      then []
      else [a]
    trimLastLine [] = []

    removeIndentation :: [String] -> [String]
    removeIndentation ys = map (dropSpaces indentation) ys
      where
        dropSpaces 0 s = s
        dropSpaces n (' ' : r) = dropSpaces (n - 1) r
        dropSpaces _ s = s
        indentation = minimalIndentation ys
        minimalIndentation =
            minimum
          . map (length . takeWhile (== ' '))
          . removeEmptyLines
        removeEmptyLines = filter (not . isEmptyLine)

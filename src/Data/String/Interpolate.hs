{-# LANGUAGE TemplateHaskell #-}
module Data.String.Interpolate (
-- * String interpolation done right
-- |
-- The examples in this module use `QuasiQuotes`.  Make sure to enable the
-- corresponding language extension.
--
-- >>> :set -XQuasiQuotes
-- >>> import Data.String.Interpolate
  i,
  normalizeLines,
) where

import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           Language.Haskell.Meta.Parse.Careful (parseExp)
import           Control.Arrow
import           Data.Char
import           Data.List

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

-- |
-- Normalizes multiline texts. Strips all surrounding whitespace
-- and newlines and removes indentation as much as possible while
-- preserving relative indentation levels.
normalizeLines :: String -> String
normalizeLines string =
    (lines >>>
     -- strip empty lines at start and end
     dropWhile (all isSpace) >>> reverse >>> dropWhile null >>> reverse >>>
     -- strip indentation
     stripIndentation >>>
     -- concatenate lines together
     intercalate "\n") string
  where
    stripIndentation :: [String] -> [String]
    stripIndentation ls =
        map (drop (minimalIndentation ls)) ls
    minimalIndentation :: [String] -> Int
    minimalIndentation =
      filter (any (not . isSpace)) >>>
      map (length . takeWhile (== ' ')) >>>
      minimum

module Grammars.AnBmAnBm where

import Data.Set (Set)
import qualified Data.Set as Set

import Grammar

g :: Grammar Char Char
g = grammar (Set.fromList "SAB") (Set.fromList "ab") rules 'S'
  where
    rules = Set.fromList $
        [ "S" := "abab"
        , "ba" := "bbBa"
        , "Ba" := "aB"
        , "Bb" := "bb"
        , "ba" := "bAaa"
        , "bA" := "Ab"
        , "aA" := "aa"
        ]

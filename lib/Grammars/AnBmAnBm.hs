module Grammars.AnBmAnBm where

import Data.Set (Set)
import qualified Data.Set as Set

import Grammar

data N = S | A | B deriving (Eq, Ord)

g :: Grammar N Char
g = grammar [S, A, B] ['a', 'b'] rules S
  where
    rules = Set.fromList $
        [ [Left S] := fmap Right "bab"
        , fmap Right "ba" := [Right 'b', Right 'b', Left B, Right 'a']
        , [Left B, Right 'a'] := [Right 'a', Left B]
        , [Left B, Right 'b'] := fmap Right "bb"
        ]

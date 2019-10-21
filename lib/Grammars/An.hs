module Grammars.An where

import Data.Set (Set)
import qualified Data.Set as Set

import Grammar

g :: Grammar Char Char
g = Grammar
    { nonTerminals = Set.fromList "a"
    , terminals    = Set.fromList "a"
    , rules = Set.fromList
              [ [Left 'a'] := [Right 'a', Left 'a']
              , [Left 'a'] := [Right 'a']
              ]
    , start = 'a'
    }

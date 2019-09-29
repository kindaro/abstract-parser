module Main where

import Data.Set (Set)
import qualified Data.Set as Set

import Grammar
import Generator

exampleGrammar :: Grammar Char Char
exampleGrammar = Grammar
    { nonTerminals = Set.fromList "a"
    , terminals    = Set.fromList "a"
    , rules = Set.fromList
              [ [Left 'a'] := [Right 'a', Left 'a']
              , [Left 'a'] := [Right 'a']
              ]
    , start = 'a'
    }

exampleGrammar2 :: Grammar String String
exampleGrammar2 = Grammar
    { nonTerminals = Set.fromList ["names", "commaNames", "andName", "finalName", "period"]
    , terminals    = Set.fromList []
    , rules = Set.fromList
              [ [Left "names"] := [Right "Nasiba", Left "commaNames"]
              , [Left "names"] := [Right "Murat", Left "commaNames"]
              , [Left "names"] := [Right "Anastasia", Left "commaNames"]
              , [Left "commaNames"] := [Right ", ", Left "names"]
              , [Left "commaNames"] := [Right " and ", Left "finalName"]
              , [Left "finalName"] := [Right "Nasiba", Left "period"]
              , [Left "finalName"] := [Right "Murat", Left "period"]
              , [Left "finalName"] := [Right "Anastasia", Left "period"]
              , [Left "period"] := [Right " have some tea."]
              ]
    , start = "names"
    }

main :: IO ()
main = print $ take 10 (generate exampleGrammar)

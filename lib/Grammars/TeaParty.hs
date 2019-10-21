module Grammars.TeaParty where

import Data.Set (Set)
import qualified Data.Set as Set

import Grammar

teaParty :: Grammar String String
teaParty = grammar nonTerminals terminals rules start
  where
    nonTerminals = Set.fromList ["names", "commaNames", "andName", "finalName", "period"]
    terminals    = Set.fromList ["Nasiba", "Murat", "Anastasia", ", ", " and ", " have some tea."]
    rules = Set.fromList
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
    start = "names"

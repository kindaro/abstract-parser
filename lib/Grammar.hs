module Grammar where

import Data.String
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.Either
import Control.Applicative
import qualified Data.List as List

data Grammar nonTerminal terminal = Grammar
    { nonTerminals :: Set nonTerminal
    , terminals :: Set terminal
    , rules :: Set (Rule nonTerminal terminal)
    , start :: nonTerminal
    } deriving (Show, Eq)

data Rule nonTerminal terminal =
    SententialForm nonTerminal terminal := SententialForm nonTerminal terminal
  deriving (Show, Eq, Ord)

leftSide, rightSide :: Rule nonTerminal terminal -> SententialForm nonTerminal terminal
leftSide (x := _) = x
rightSide (_ := y) = y

grammar :: (Ord nonTerminal, Ord terminal)
        => Set nonTerminal
        -> Set terminal
        -> Set (Rule nonTerminal terminal)
        -> nonTerminal
        -> Grammar nonTerminal terminal
grammar nonTerminals terminals rules start
    | looksGood = Grammar {..}
    | otherwise = error "The grammar does not look good."
  where
    symbols = Set.map Left nonTerminals `Set.union` Set.map Right terminals
    looksGood = start `Set.member` nonTerminals
                && (Set.fromList . concatMap leftSide) rules `Set.isSubsetOf` symbols
                && (Set.fromList . concatMap rightSide) rules `Set.isSubsetOf` symbols

type SententialForm nonTerminal terminal = [Either nonTerminal terminal]

type Sentence terminal = [terminal]

instance {-# overlapping #-} IsString (SententialForm Char Char) where
    fromString = catMaybes . fmap fromChar
        where fromChar c =
                  if isUpper c then (Just . Left) c
                     else if isLower c then (Just . Right) c else Nothing

maybeSentence :: SententialForm nonTerminal terminal -> Maybe (Sentence terminal)
maybeSentence xs = case partitionEithers xs of
    ([ ], ys) -> Just ys
    _         -> Nothing

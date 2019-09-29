module Generator where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Maybe
import Data.Either
import Data.Function

import Grammar

generate :: (Eq terminal, Eq nonTerminal)
         => Grammar nonTerminal terminal -> [Sentence terminal]
generate g@Grammar{..} = fix (generate' g) [[Left start]]

generate' :: forall terminal nonTerminal. (Eq terminal, Eq nonTerminal)
          => Grammar nonTerminal terminal
          -> ( [SententialForm nonTerminal terminal] -> [Sentence terminal] )
          -> [SententialForm nonTerminal terminal]
          -> [Sentence terminal]
generate' _ _ [ ] = [ ]
generate' g@Grammar{..} f q = sentences ++ f sententialForms
  where
    sententialForms :: [SententialForm nonTerminal terminal]
    sentences :: [Sentence terminal]
    (sententialForms, sentences) = partitionEithers . fmap eitherSentence $ q'

    q' :: [SententialForm nonTerminal terminal]
    q' = do
        rule <- Set.toList rules
        sf <- q
        expandSententialForm rule sf

expandSententialForm :: (Eq terminal, Eq nonTerminal)
                     => Rule nonTerminal terminal
                     -> SententialForm nonTerminal terminal
                     -> [SententialForm nonTerminal terminal]
expandSententialForm rule = catMaybes . fmap (fmap deselect . traverse (maybeApplyRule rule)) . subSequencesInContext

eitherSentence :: SententialForm nonTerminal terminal
               -> Either (SententialForm nonTerminal terminal) (Sentence terminal)
eitherSentence s = maybe (Left s) Right . maybeSentence $ s

maybeApplyRule :: (Eq terminal, Eq nonTerminal)
               => Rule nonTerminal terminal
               -> SententialForm nonTerminal terminal
               -> Maybe (SententialForm nonTerminal terminal)
maybeApplyRule rule s | s == leftSide rule = Just (rightSide rule)
                      | otherwise = Nothing

type Selection a = (([a], [a]), [a])

subSequencesInContext :: [a] -> [Selection a]
subSequencesInContext xs =
  let n = length xs
  in [ select i j xs | i <- [0.. n], j <- [i + 1.. n] ]

select :: Int -> Int -> [a] -> Selection a
select i j xs =
  let (before, s) = splitAt i xs
      (selection, after) = splitAt (j - i) s
  in ((before, after), selection)

deselect :: Selection a -> [a]
deselect ((before, after), selection) = concat [before, selection, after]

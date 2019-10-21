module Generator where

import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Maybe
import Data.Either
import Data.Function

import Grammar
import Converge

type G = Grammar
type S σ = Sentence σ
type SF ν σ = SententialForm ν σ

generate :: (Eq σ, Eq ν, Show σ, Show ν) => G ν σ -> [Either String (S σ)]
generate g@Grammar{..} = fix (generate' g) [[Left start]]

generate' :: forall σ ν. (Eq σ, Eq ν, Show σ, Show ν) => G ν σ -> ( [SF ν σ] -> [Either String (S σ)] ) -> [SF ν σ] -> [Either String (S σ)]
generate' _ _ [ ] = [ ]
generate' g@Grammar{..} f q =
    (Left "... iteration ...": fmap Right sentences)
    ++ (Left "--- in works ---": fmap (Left . show) sententialForms)
    ++ f (List.nub sententialForms)
    ++ f ((List.nub . (fmap.fmap) Right) sentences)
  where
    sententialForms :: [SF ν σ]
    sentences :: [S σ]
    (sententialForms, sentences) = partitionEithers . fmap eitherS $ q'

    q' :: [SF ν σ]
    q' = do
        rule <- Set.toList rules
        sf <- q
        expandSF rule sf

expandSF :: (Eq σ, Eq ν) => Rule ν σ -> SF ν σ -> [SF ν σ]
expandSF rule = catMaybes . fmap (fmap deselect . traverse (maybeApplyRule rule)) . subSequencesInContext

eitherS :: SF ν σ -> Either (SF ν σ) (S σ)
eitherS s = maybe (Left s) Right . maybeSentence $ s

maybeApplyRule :: (Eq σ, Eq ν) => Rule ν σ -> SF ν σ -> Maybe (SF ν σ)
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
deselect ((before, after), selection) = concat @[] [before, selection, after]

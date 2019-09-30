module Zipper where

import Data.Foldable
import qualified Data.List as List
import Data.Maybe

import Text.PrettyPrint.Boxes

data Zipper a = Zipper { lefts, rights :: [a], focus :: a } deriving Eq

integers :: Integral i => Zipper i
integers = Zipper { lefts = [-1, -2..], focus = 0, rights = [1..] }

instance Functor Zipper where
    fmap f Zipper{..} = Zipper { lefts = fmap f lefts, focus = f focus, rights = fmap f rights }

instance Foldable Zipper where
    foldr f x Zipper{..} = foldr f x (focus: (concat . List.transpose) [lefts, rights])

instance Show a => Show (Zipper a) where
    show z@Zipper{..} = (render . boxify) z
      where
        cellX = maximum . fmap (maximum . fmap length . lines . show) $ z
        cellY = maximum . fmap (length . lines . show) $ z
        boxify = tabulate cellX cellY (length lefts) (length rights) id

frame :: Box -> Box
frame x = hcat @[] center1 [vstick, hstick // x // hstick, vstick]
  where
    vstick = "+" // vcat center1 (replicate (rows x) "|") // "+"
    hstick = hcat center1 (replicate (cols x) "-")

tabulate :: Show a => Int -> Int -> Int -> Int -> (Box -> Box) -> Zipper a -> Box
tabulate cellX cellY leftwards rightwards decorate
    = hsep 1 center1 . straighten
    . padLeft leftwards (normalize' nullBox) . padRight rightwards (normalize' nullBox)
    . fmap (decorate . normalize . text)
    . fmap show
  where normalize  = align center1 center1 cellY cellX
        normalize' = align center1 center1 (cellY + (rows . decorate) nullBox)
                                                (cellX + (cols . decorate) nullBox)

padLeft, padRight, pad :: Int -> a -> Zipper a -> Zipper a
padLeft  n x Zipper{..} = Zipper { lefts  = lefts  ++ replicate (n - length lefts)  x, .. }
padRight n x Zipper{..} = Zipper { rights = rights ++ replicate (n - length rights) x, .. }
pad n x = padLeft n x . padRight n x

stepLeft, stepRight :: Zipper a -> Zipper a
stepLeft  Zipper{..} = Zipper { lefts  = tail lefts , focus = head lefts , rights = focus: rights }
stepRight Zipper{..} = Zipper { rights = tail rights, focus = head rights, lefts  = focus: lefts  }

crop :: Int -> Int -> Zipper a -> Zipper a
crop m n Zipper{..} = Zipper { lefts = take m lefts, focus = focus, rights = take n rights }

straighten :: Zipper a -> [a]
straighten Zipper{..} = reverse lefts ++ focus: rights

mirror :: [a] -> Maybe (Zipper a)
mirror [ ] = Nothing
mirror (x: xs) = Just Zipper { lefts = xs, focus = x, rights = xs }

type Zipper2D a = Zipper (Zipper a)

instance {-# overlapping #-} Show a => Show (Zipper (Zipper a)) where
    show z = render . vcat left . straighten . fmap boxify $ z
      where
        xs = concatMap toList z
        cellX = maximum . fmap (maximum . fmap length . lines . show) $ xs
        cellY = maximum . fmap (               length . lines . show) $ xs
        leftwards  = maximum . fmap (length . lefts ) $ z
        rightwards = maximum . fmap (length . rights) $ z
        boxify = tabulate cellX cellY leftwards rightwards id

stepUp, stepDown :: Zipper2D a -> Zipper2D a
stepUp   zs = fmap stepLeft  zs
stepDown zs = fmap stepRight zs

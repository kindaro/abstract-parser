module Zipper where

import Data.Foldable
import qualified Data.List as List
import Data.Maybe

import Text.PrettyPrint.Boxes

data Zipper a = Zipper { lefts, rights :: [a], focus :: a } deriving Eq

integers :: Integral i => Zipper i
integers = Zipper { lefts = [-1, -2..], focus = 0, rights = [1..] }

singleton :: a -> Zipper a
singleton x = Zipper { lefts = [ ], focus = x, rights = [ ] }

mirror :: a -> [a] -> Zipper a
mirror x xs = Zipper { lefts = xs, focus = x, rights = xs }

instance Functor Zipper where
    fmap f Zipper{..} = Zipper { lefts = fmap f lefts, focus = f focus, rights = fmap f rights }

instance Foldable Zipper where
    foldr f x Zipper{..} = foldr f x (focus: (concat . List.transpose) [lefts, rights])

instance Show a => Show (Zipper a) where
    show z@Zipper{..} = (render . boxify) z
      where
        cellX = maximum . fmap (maximum . fmap length . lines . show) $ z
        cellY = maximum . fmap (length . lines . show) $ z
        boxify = tabulate show cellX cellY (length lefts) (length rights) id

data X = X | Y
instance Show X where show X = "la\nla"; show Y = "fa"
z1 = Zipper { lefts = [X], focus = Y, rights = [ ] }

frame :: Box -> Box
frame x = hcat @[] center1 [vstick, hstick // x // hstick, vstick]
  where
    vstick = "+" // vcat center1 (replicate (rows x) "|") // "+"
    hstick = hcat center1 (replicate (cols x) "-")

box :: String -> Box
box [ ] = nullBox
box s = vcat left . fmap (alignHoriz left width . text) $ xs
  where
    xs = lines s
    width = (maximum . fmap length) xs

tabulate :: (a -> String) -> Int -> Int -> Int -> Int -> (Box -> Box) -> Zipper a -> Box
tabulate show' cellX cellY leftwards rightwards decorate
    = hsep 1 center1 . straighten
    . padLeft leftwards (normalize' nullBox) . padRight rightwards (normalize' nullBox)
    . fmap (decorate . normalize . text)
    . fmap show'
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

growLeft, growRight :: a -> Zipper a -> Zipper a
growLeft  x z@Zipper{..} | null lefts  = z { focus = x, rights = focus: rights }
                         | otherwise = stepLeft  z
growRight x z@Zipper{..} | null rights = z { focus = x, lefts  = focus: lefts  }
                         | otherwise = stepRight z

crop :: Int -> Int -> Zipper a -> Zipper a
crop m n Zipper{..} = Zipper { lefts = take m lefts, focus = focus, rights = take n rights }

straighten :: Zipper a -> [a]
straighten Zipper{..} = reverse lefts ++ focus: rights

type Zipper2D a = Zipper (Zipper a)

singleton2d :: a -> Zipper2D a
singleton2d = singleton . singleton

tabulate2d :: (a -> String) -> Zipper2D a -> String
tabulate2d show z = render . vcat left . straighten . fmap boxify $ z
      where
        xs = concatMap toList z
        cellX = maximum . fmap (maximum . fmap length . lines . show) $ xs
        cellY = maximum . fmap (               length . lines . show) $ xs
        leftwards  = maximum . fmap (length . lefts ) $ z
        rightwards = maximum . fmap (length . rights) $ z
        boxify = tabulate show cellX cellY leftwards rightwards id


instance {-# overlapping #-} Show a => Show (Zipper (Zipper a)) where
    show z = tabulate2d show z

z2 = let (x: xs) = [ crop i (5 - i) integers | i <- [0..5] ] in mirror x xs

stepLeft2D, stepRight2D, stepUp2D, stepDown2D :: Zipper2D a -> Zipper2D a
stepLeft2D  = fmap stepLeft
stepRight2D = fmap stepRight
stepUp2D   = stepLeft
stepDown2D = stepRight

growLeft2D, growRight2D, growUp2D, growDown2D :: a -> Zipper2D a -> Zipper2D a
growLeft2D  x = fmap (growLeft  x)
growRight2D x = fmap (growRight x)
growUp2D   x = growLeft  (singleton x)
growDown2D x = growRight (singleton x)

extend :: (Zipper a -> a) -> Zipper a -> Zipper a
extend f z = z { focus = f z }

extract :: Zipper a -> a
extract = focus

extend2d :: (Zipper2D a -> a) -> Zipper2D a -> Zipper2D a
extend2d f z = z { focus = extend (const (f z)) (focus z) }

extract2d :: Zipper2D a -> a
extract2d = focus . focus

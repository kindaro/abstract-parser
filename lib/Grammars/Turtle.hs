module Grammars.Turtle where

import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Text.PrettyPrint.Boxes
import Data.Function

import Zipper (Zipper(..), Zipper2D)
import qualified Zipper as Zipper
import Grammar

data Direction = North | East | South | West deriving (Show, Eq, Ord, Enum, Bounded)
data Move = Move deriving (Show, Eq, Ord)

chrDirection :: Direction -> Char
chrDirection North = '⭡'
chrDirection East  = '⭢'
chrDirection South = '⭣'
chrDirection West  = '⭠'

growDirection :: a -> Direction -> Zipper2D a -> Zipper2D a
growDirection x North = Zipper.growUp2D    x
growDirection x East  = Zipper.growRight2D  x
growDirection x South = Zipper.growDown2D  x
growDirection x West  = Zipper.growLeft2D x

turtle :: Grammar Move Direction
turtle = grammar [Move] [minBound..] rules Move
  where
    rules = Set.fromList $
        [ [Left Move] := [Right North, Left Move, Right South]
        , [Left Move] := [Right East, Left Move, Right West]
        , [Left Move] := [ ]
        ] ++ [ [Right x, Right y]
             := [Right y, Right x] | x <- [minBound..], y <- List.delete x [minBound..] ]

drawTurtle :: Sentence Direction -> [String]
drawTurtle s = fmap (Zipper.tabulate2d pure) $ List.scanl walk (Zipper.singleton2d '+') s
  where
    walk :: Zipper2D Char -> Direction -> Zipper2D Char
    walk z x = z & growDirection ' ' x
                 & Zipper.extend2d (const (chrDirection x))
                 & growDirection ' ' x
                 & Zipper.extend2d (\z -> if Zipper.extract2d z == '+' then '+' else '*')

renderTurtle :: Sentence Direction -> String
renderTurtle = render . Zipper.frame . Zipper.box . last . drawTurtle

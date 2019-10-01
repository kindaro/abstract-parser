module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable
import qualified Data.List as List
import Data.Function

import Text.PrettyPrint.Boxes

import Zipper (Zipper(..), Zipper2D)
import qualified Zipper as Zipper
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

main :: IO ()
main = do
    -- print $ take 10 (generate exampleGrammar)
    -- traverse_ (putStrLn . concat) . take 10 . generate $ teaParty
    -- traverse_ print . take 10 . drop 100 . generate $ turtle
    traverse_ ((\(x, (y, z)) -> print x >> print y >> putStrLn z) . fmap (\x -> (x, renderTurtle x)))
        . take @(Int, Sentence Direction) 1000 . drop 10000 $ zip [1 :: Int ..] (generate turtle)

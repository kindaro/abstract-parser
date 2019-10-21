module Main where

import Data.Foldable

import Generator

import qualified Grammars.AnBmAnBm as AnBmAnBm

main :: IO ()
main = do
    -- print $ take 10 (generate exampleGrammar)
    -- traverse_ (putStrLn . concat) . take 10 . generate $ teaParty
    -- traverse_ print . take 10 . drop 100 . generate $ turtle
    traverse_ print . take 10000 $ (generate AnBmAnBm.g)

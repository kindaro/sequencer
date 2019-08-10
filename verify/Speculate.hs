module Main where

import Test.Speculate

import Control.Sequencer
import Data.List
import Data.Foldable (asum)

main :: IO ()
main = speculate args
  { constants =
      [ constant "serialize"
            (serialize :: ([Int] -> [[Int]]) -> [[Int]] -> [[Int]])
      , constant "serialize-mb"  
            (serialize :: (Maybe Int -> Maybe [Int]) -> [Maybe Int] -> (Maybe [Int]))
      , constant "serialize-mb-2"  
            (serialize :: ([Int] -> [Maybe Int]) -> Maybe [Int] -> [Maybe Int])
      , background
      , constant "id" (id :: [Int] -> [Int])
      , constant "transpose" (transpose @Int)
      , constant "sequence" (sequence @[] @[] @Int)
      , constant "sequence-mb" (sequence @[] @Maybe @Int)
      , constant "sequence-mb'" (sequence @Maybe @[] @Int)
      , constant "asum-mb" (asum :: Maybe [Int] -> [Int])
      , constant "[ ]" ([ ] @Int)
      , constant "[1]" ([1] :: [Int])
      , constant "[[ ]]" ([[ ]] :: [[Int]])
      , constant "[[1]]" ([[1]] :: [[Int]])
      , constant "Nothing-1" (Nothing :: Maybe Int)
      , constant "Nothing-2" (Nothing :: Maybe [Int])
      , constant "reverse" (reverse @Int)
      , constant "reverse" (reverse @[Int])
      , constant "pure-1" (pure :: Int -> [Int])
      , constant "pure-1-mb" (pure :: Int -> (Maybe Int))
      , constant "pure-2" (pure :: [Int] -> [[Int]])
      , constant "pure-2-mb1" (pure :: [Int] -> (Maybe [Int]))
      , constant "pure-2-mb2" (pure :: (Maybe Int) -> [Maybe Int])
      , constant "fmap-1" (fmap :: (Int -> [Int]) -> [Int] -> [[Int]])
      , constant "fmap-2" (fmap :: ([Int] -> [Int]) -> [[Int]] -> [[Int]])
      , constant "fmap-2-mb" (fmap :: ((Maybe Int) -> (Maybe Int)) -> [Maybe Int] -> [Maybe Int])
      , constant "fmap-1-mb" (fmap :: (Int -> (Maybe Int)) -> [Int] -> [Maybe Int])
      , constant "fmap-3-mb" (fmap :: ([Int] -> [[Int]]) -> Maybe [Int] -> Maybe [[Int]])
      ]
  , instances =
        [ reifyInstances (undefined :: Maybe Int)
        , reifyInstances (undefined :: Maybe [Int])
        , reifyInstances (undefined :: [[Int]])
        ]
  , maxSize = 6
  , maxVars = 4
  , maxCondSize = 5
  , maxTests = 35
  , showConditions = True
  -- , showSemiequations = False
  }


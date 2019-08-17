module Main where

import Test.Tasty
import Test.Tasty.QuickCheck

import Redundant


main :: IO ()
main = defaultMain $

    testProperty "redundant can be retracted" prop_redundant_all

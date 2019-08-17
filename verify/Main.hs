module Main where

import Test.Tasty
import Test.Tasty.QuickCheck

import Redundant
import Assets


main :: IO ()
main = defaultMain $ testGroup "Laws."
    [ testProperty "example action can be retracted"
        \x -> (retractExampleAction . fst . fst) (runM @Integer exampleAction [x]) == Just x
    , testProperty "`redundant` can be retracted" prop_redundant_all
    ]

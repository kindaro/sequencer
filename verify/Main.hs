module Main where

import Data.List (genericReplicate)

import Control.Sequencer
import Control.Sequencer.Internal
import Test.Tasty

import qualified Internal
import qualified External

main :: IO ()
main = do

    -- Do this:
    --
    -- 1. Run `insistent` many times over randomly failing action.

    defaultMain $ Internal.checks

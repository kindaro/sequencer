module Main where

import Control.Sequencer
import Test.Tasty
import Test.Tasty.LeanCheck

-- import qualified Internal
-- import qualified External

main :: IO ()
main = do

    -- Do this:
    --
    -- 1. Run `insistent` many times over randomly failing action.

    defaultMain $

    -- Laws:

        testGroup "Laws of `serialize`:"
            [ testProperty "serialize (fmap pure) == sequence -- with Maybe [Int]"
                \mxs -> serialize (fmap pure) mxs == sequence @Maybe (mxs :: Maybe [Int])
            , testProperty "serialize (fmap pure) == sequence -- with [Maybe Int]"
                \mxs -> serialize (fmap pure) mxs == sequence @[] (mxs :: [Maybe Int])
            ]

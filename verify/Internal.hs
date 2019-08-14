module Internal where

import Control.Sequencer.Internal
import Test.Tasty
import Test.Tasty.HUnit

checks = testGroup "alternativeTranscode"
    [ testCase "transpose a list"
        $ assertEqual "" [[1,2,3]]
            $ alternativeTranscode (fmap pure) [[1],[2],[3]]
    , testCase "transpose any list"
        $ undefined
    ]


module Internal where

import Control.Sequencer.Internal
import Test.Tasty
import Test.Tasty.HUnit

checks = testGroup ""
    [ testCase "" $ assertEqual "" [[1,2,3]] $ alternativeTranscode @[] @[] (fmap pure) [[1],[2],[3]]
    ]


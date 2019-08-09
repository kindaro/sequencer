module Main where

import Data.List (genericReplicate)

import Control.Sequencer
import Control.Sequencer.Internal

-- | Run the action many times and verify that every possible result is returned at least once.
-- With sufficient `n`, the probability of every possible result actually being returned may
-- approach 100% as close as we would like.
probabilisticCheck :: (Eq a, Monad m) => Word -> [a] -> m a -> m [(a, Bool)]
probabilisticCheck n cases u = do
    rs <- (sequence . genericReplicate n) u
    return $ fmap (\x -> (x, x `elem` rs)) cases

main :: IO ()
main = do
    undefined

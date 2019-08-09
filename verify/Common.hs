module Common where

import Control.Exception (Exception, displayException)
import Control.Monad.Catch (throwM)
import Data.List (genericReplicate)
import System.Random

data CheckFailure = CheckFailure deriving Show
instance Exception CheckFailure where displayException _ = "check failure"

-- | Run the action many times and verify that every possible result is returned at least once.
-- With sufficient `n`, the probability of every possible result actually being returned may
-- approach 100% as close as we would like.
probabilisticCheck :: (Eq a, Monad m) => Word -> [a] -> m a -> m [(a, Bool)]
probabilisticCheck n cases u = do
    rs <- (sequence . genericReplicate n) u
    return $ fmap (\x -> (x, x `elem` rs)) cases

sometimesFail p = do
    x <- randomRIO (1, 100)
    if x <= p
       then return ()
       else throwM CheckFailure

-- λ runWriterT $ insistent 10 (lift oftenFail) :: IO ((), Proxy SomeException)
-- ((),Proxy)
-- λ runWriterT $ insistent 10 (lift oftenFail) :: IO ((), Proxy SomeException)
-- *** Exception: SequencingFailure

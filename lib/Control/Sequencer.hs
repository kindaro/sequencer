module Control.Sequencer
    ( independent
    , independent'
    , interleaved
    , insistent
    , redundant
    , SequencingFailure
    ) where

import Control.Exception (Exception, SomeException, SomeAsyncException, displayException)
import Control.Monad.Catch (MonadCatch, Handler(..), throwM, catches)
import Control.Monad.Writer.Strict (MonadWriter, tell)
import Control.Applicative (Alternative, empty)
import Data.Foldable (asum)
import Data.List (genericReplicate)
import Data.Sequence (Seq)

data SequencingFailure = SequencingFailure deriving Show
instance Exception SequencingFailure where displayException _ = "sequencing failure"

-- | Run all the actions independently, collect the results in a list and log the errors in a
-- sequence.
independent :: forall a m ins. (MonadWriter (Seq SomeException) m, MonadCatch m, Traversable ins)
            => ins (m a) -> m [a]
independent = independent' (tell . pure)

-- | Run all the actions independently, collect the results and log the errors with the supplied
-- logger.
independent' :: forall a m e ins outs.
                    (Exception e, MonadCatch m, Traversable ins, Alternative outs)
             => (e -> m ()) -> ins (m a) -> m (outs a)
independent' logger = fmap asum . sequence . fmap f
    where f u = fmap pure u `catchSynchronous` \e -> logger e *> return empty

interleaved :: MonadCatch m => [m a] -> m [Either SomeException a]
interleaved [ ] = return [ ]
interleaved (x: xs) = do
    r <- fmap (pure . Right) x `catchSynchronous` \e -> (pure . pure . Left) (e:: SomeException)
    fmap (r ++) (interleaved xs)

insistent :: (MonadCatch m, MonadWriter (q SomeException) m, Alternative q)
          => Word -> m a -> m a
insistent n = redundant . genericReplicate n

redundant :: (MonadCatch m, MonadWriter (q SomeException) m, Foldable f, Alternative q)
          => f (m a) -> m a
redundant = foldr1 f where f x y = x `logSynchronous` const y

-- Log everything going in and out, and provide the same for analysis.
--
-- Logging is a monadic action, so loggers can be sewn inside the environment.
--
-- We must edit the process description to launch the `timeout` program available on Unix.
--
-- transparent :: ProcessDescription a b c -> 

catchSynchronous :: forall e m a. (Exception e, MonadCatch m) => m a -> (e -> m a) -> m a
catchSynchronous action handler = action `catches`
    [ Handler (throwM :: SomeAsyncException -> m w), Handler handler ]

logSynchronous :: (Exception e, MonadCatch m, MonadWriter (q e) m, Alternative q)
                 => m a -> (e -> m a) -> m a
logSynchronous action handler = catchSynchronous action (\e -> (tell . pure) e *> handler e)

-- rarelyFail = randomRIO (1 :: Word, 10) >>= \x -> if x == 7 then throw Overflow else return ()
-- oftenFail = randomRIO (1 :: Word, 10) >>= \x -> if x /= 7 then throw Overflow else return ()

-- λ runWriterT $ insistent 10 (lift oftenFail) :: IO ((), Proxy SomeException)
-- ((),Proxy)
-- λ runWriterT $ insistent 10 (lift oftenFail) :: IO ((), Proxy SomeException)
-- *** Exception: SequencingFailure

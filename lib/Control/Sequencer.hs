module Control.Sequencer
    ( independent
    , independent'
    , interleaved
    , insistent
    , redundant
    , SequencingFailure
    ) where

import Control.Exception (Exception, SomeException)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Writer.Strict (MonadWriter, tell)
import Control.Applicative (Alternative, empty)
import Data.List (genericReplicate)
import Data.Sequence (Seq)

import Control.Sequencer.Internal

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
independent' logger = alternativeTranscode f
    where f u = fmap pure u `catchSynchronous` \e -> logger e *> return empty

interleaved :: forall a m e ins outs.
                (Exception e, MonadCatch m, Traversable ins, Alternative outs)
            => ins (m a) -> m (outs (Either e a))
interleaved = alternativeTranscode (fmap pure . trySynchronous)

insistent :: (MonadCatch m, MonadWriter (q SomeException) m, Alternative q)
          => Word -> m a -> m a
insistent n = redundant . genericReplicate n

redundant :: (MonadCatch m, MonadWriter (q SomeException) m, Foldable f, Alternative q)
          => f (m a) -> m a
redundant = foldr1 f where f x y = x `logSynchronous` const y

-- rarelyFail = randomRIO (1 :: Word, 10) >>= \x -> if x == 7 then throw Overflow else return ()
-- oftenFail = randomRIO (1 :: Word, 10) >>= \x -> if x /= 7 then throw Overflow else return ()

-- λ runWriterT $ insistent 10 (lift oftenFail) :: IO ((), Proxy SomeException)
-- ((),Proxy)
-- λ runWriterT $ insistent 10 (lift oftenFail) :: IO ((), Proxy SomeException)
-- *** Exception: SequencingFailure

module Control.Sequencer
    ( independent
    -- , insistent
    , redundant
    , trySynchronous
    , triesSynchronous
    , SequencingFailure(..)
    ) where

import Control.Exception (Exception, SomeException(..))
import Control.Monad.Catch (MonadCatch, throwM, Handler)
import Control.Monad.Writer.Strict (MonadWriter, tell)
import Control.Applicative (Alternative, empty)
import Data.List (genericReplicate)
import Control.Exception (displayException)

import Control.Sequencer.Internal

data SequencingFailure = SequencingFailure deriving Show
instance Exception SequencingFailure where displayException _ = "sequencing failure"

-- | Run all the actions independently from each other, collect the results and log the errors
-- with the supplied logger.
independent :: forall a m e ins outs.
                    (Exception e, MonadCatch m, Traversable ins, Alternative outs)
            => (e -> m ()) -> ins (m a) -> m (outs a)
independent logger = undefined
    where f u = fmap pure u `catchSynchronous` \e -> logger e *> return empty

-- insistent :: (MonadCatch m, MonadWriter (q SomeException) m, Alternative q)
--           => Word -> m a -> m a
-- insistent n = redundant . genericReplicate n

redundant :: forall e m f q a. (MonadCatch m, Foldable f)
          => [Handler m e] -> (e -> m ()) -> f (m a) -> m a
redundant handlers logger = foldr f (throwM SequencingFailure)
  where
    f :: m a -> m a -> m a
    f x y = triesSynchronous handlers x >>= either (\e -> logger e >> y) return

trySynchronous :: forall e m a. (Exception e, MonadCatch m) => m a -> m (Either e a)
trySynchronous x = fmap Right x `catchSynchronous` (return . Left)

triesSynchronous :: forall m a b. MonadCatch m => [Handler m b] -> m a -> m (Either b a)
triesSynchronous handlers action =
    fmap Right action `catchesSynchronous` (fmap . fmap) Left handlers

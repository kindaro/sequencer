module Control.Sequencer
    ( independent
    , independent_
    , insistent
    , insistent_
    , redundant
    , redundant_
    , trySynchronous
    , triesSynchronous
    , SequencingFailure(..)
    ) where

import Control.Exception (Exception, SomeException(..))
import Control.Monad.Catch (MonadCatch, throwM, Handler(..))
import Control.Applicative (Alternative, empty)
import Data.List (genericReplicate)
import Control.Exception (displayException)
import Control.Applicative

import Control.Sequencer.Internal

data SequencingFailure = SequencingFailure deriving Show
instance Exception SequencingFailure where displayException _ = "sequencing failure"

-- | Run all the actions independently from each other, collect the results and log the errors
-- with the supplied logger.
independent :: forall a m e ins outs.
                    (MonadCatch m, Traversable ins, Alternative outs)
            => [Handler m e] -> (e -> m ()) -> ins (m a) -> m (outs a)
independent handlers logger = foldr f (return empty)
  where
    f :: m a -> m (outs a) -> m (outs a)
    f x ys = do
              r <- triesSynchronous handlers x
              case r of
                  Left e -> logger e >> ys
                  Right v -> fmap (pure v <|>) ys

independent_ :: forall m ins outs a. (MonadCatch m, Traversable ins, Alternative outs)
             => ins (m a) -> m (outs a)
independent_ = independent [defaultHandler] defaultLogger

insistent :: forall e m n a . (Integral n, MonadCatch m)
          => [Handler m e] -> (e -> m ()) -> n -> m a -> m a
insistent handlers logger n = redundant handlers logger . genericReplicate n

insistent_ :: (Integral n, MonadCatch m) => n -> m a -> m a
insistent_ n = redundant_ . genericReplicate n

redundant :: forall e m f a. (MonadCatch m, Foldable f)
          => [Handler m e] -> (e -> m ()) -> f (m a) -> m a
redundant handlers logger = foldr f (throwM SequencingFailure)
  where
    f :: m a -> m a -> m a
    f x y = triesSynchronous handlers x >>= either (\e -> logger e >> y) return

-- | Log to nowhere and ignore all synchronous exceptions.
redundant_ :: forall m f a. (MonadCatch m, Foldable f) => f (m a) -> m a
redundant_ = redundant [defaultHandler] defaultLogger

defaultLogger :: forall m a. Monad m => a -> m ()
defaultLogger = const (return ())

defaultHandler :: forall m. Monad m => Handler m ()
defaultHandler = Handler handleAll
  where
    handleAll :: SomeException -> m ()
    handleAll _ = return ()

trySynchronous :: forall e m a. (Exception e, MonadCatch m) => m a -> m (Either e a)
trySynchronous x = fmap Right x `catchSynchronous` (return . Left)

triesSynchronous :: forall m a b. MonadCatch m => [Handler m b] -> m a -> m (Either b a)
triesSynchronous handlers action =
    fmap Right action `catchesSynchronous` (fmap . fmap) Left handlers

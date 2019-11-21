module Control.Sequencer
    ( independent
    , independent_
    , insistent
    , insistent_
    , redundant
    , redundant_
    , trySynchronous
    , triesSynchronous
    , defaultHandler
    , defaultLogger
    , SequencingFailure(..)
    ) where

import Control.Exception (Exception, SomeException(..))
import Control.Monad.Catch (MonadCatch, throwM, Handler(..))
import Control.Applicative (Alternative, empty)
import Data.List (genericReplicate)
import Control.Exception (displayException)
import Control.Applicative
import Data.Witherable

import Control.Sequencer.Internal

data SequencingFailure = SequencingFailure deriving Show
instance Exception SequencingFailure where displayException _ = "sequencing failure"

-- | Run all the actions independently from each other, collect the results and log the errors
-- with the supplied logger.
independent :: forall a m e w.
                    (MonadCatch m, Witherable w)
            => [Handler m e] -> (e -> m ()) -> w (m a) -> m (w a)
independent handlers logger = wither f
  where
    f :: m a -> m (Maybe a)
    f mx = do
        r <- triesSynchronous handlers mx
        case r of
            Left e -> logger e *> return Nothing
            Right v -> return (Just v)

independent_ :: forall m w a. (MonadCatch m, Witherable w)
             => w (m a) -> m (w a)
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

defaultHandler :: forall m. Monad m => Handler m SomeException
defaultHandler = Handler handleAll
  where
    handleAll :: SomeException -> m SomeException
    handleAll e = return e

trySynchronous :: forall e m a. (Exception e, MonadCatch m) => m a -> m (Either e a)
trySynchronous x = fmap Right x `catchSynchronous` (return . Left)

triesSynchronous :: forall m a b. MonadCatch m => [Handler m b] -> m a -> m (Either b a)
triesSynchronous handlers action =
    fmap Right action `catchesSynchronous` (fmap . fmap) Left handlers

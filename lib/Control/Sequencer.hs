module Control.Sequencer
    ( independent
    -- , independent_
    , insistent
    -- , insistent_
    , redundant
    -- , redundant_
    , cool
    , trySynchronous
    , triesSynchronous
    , handleAllSynchronous
    , logToNowhere
    , SequencingFailure(..)
    ) where

import Control.Exception (Exception, SomeException(..), displayException)
import Control.Monad.Catch (MonadCatch, throwM, Handler(..))
import Data.List (genericReplicate)
import Data.Witherable
import Control.Monad.IO.Class
import Control.Concurrent

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

insistent :: forall e m n a . (Integral n, MonadCatch m)
          => [Handler m e] -> (e -> m ()) -> n -> m a -> m a
insistent handlers logger n = redundant handlers logger . genericReplicate n

-- | Like `insistent`, but accepts a number of milliseconds to sleep between trying.
cool :: forall e m n a . (Integral n, MonadCatch m, MonadIO m)
          => [Handler m e] -> (e -> m ()) -> Int -> n -> m a -> m a
cool handlers logger delay n x
    | n < 1 = throwM SequencingFailure 
    | otherwise = fmap (either id id) cherryOnTop
  where
    actions = redundant handlers logger (genericReplicate (n - 1) delayed)
    action  = redundant handlers logger [x]
    handleSequencingFailure = Handler @_ @_ @SequencingFailure (\_ -> action)

    -- If the action fails, suspend execution.
    delayed = do
        y <- trySynchronous @SomeException x
        case y of
            Left e -> do
                liftIO (threadDelay delay)
                throwM e
            Right z -> return z

    -- If the chain of actions and delays fails, catch `SequencingFailure` and try one last time.
    cherryOnTop = triesSynchronous [handleSequencingFailure] actions

redundant :: forall e m f a. (MonadCatch m, Foldable f)
          => [Handler m e] -> (e -> m ()) -> f (m a) -> m a
redundant handlers logger = foldr f (throwM SequencingFailure)
  where
    f :: m a -> m a -> m a
    f x y = triesSynchronous handlers x >>= either (\e -> logger e >> y) return

logToNowhere :: forall m a. Monad m => a -> m ()
logToNowhere = const (return ())

handleAllSynchronous :: forall m. Monad m => Handler m SomeException
handleAllSynchronous = Handler handleAll
  where
    handleAll :: SomeException -> m SomeException
    handleAll e = return e

trySynchronous :: forall e m a. (Exception e, MonadCatch m) => m a -> m (Either e a)
trySynchronous x = fmap Right x `catchSynchronous` (return . Left)

triesSynchronous :: forall m a b. MonadCatch m => [Handler m b] -> m a -> m (Either b a)
triesSynchronous handlers action =
    fmap Right action `catchesSynchronous` (fmap . fmap) Left handlers

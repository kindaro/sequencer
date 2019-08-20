module Control.Sequencer.Internal
    ( catchSynchronous
    , catchesSynchronous
    , rethrowAsync
    ) where

import Control.Monad.Catch (MonadCatch, Handler(..), throwM, catches)
import Control.Exception (Exception, SomeAsyncException)

catchSynchronous :: forall e m a. (Exception e, MonadCatch m) => m a -> (e -> m a) -> m a
catchSynchronous action handler = action `catches` [ rethrowAsync, Handler handler ]

rethrowAsync :: forall m a. MonadCatch m => Handler m a
rethrowAsync = Handler (throwM :: SomeAsyncException -> m a)

catchesSynchronous :: forall m a. MonadCatch m => m a -> [Handler m a] -> m a
catchesSynchronous action handlers = action `catches` (rethrowAsync: handlers)

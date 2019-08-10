module Control.Sequencer.Internal
    ( catchSynchronous
    ) where

import Control.Monad.Catch (MonadCatch, Handler(..), throwM, catches)
import Control.Exception (Exception, SomeAsyncException)

catchSynchronous :: forall e m a. (Exception e, MonadCatch m) => m a -> (e -> m a) -> m a
catchSynchronous action handler = action `catches`
    [ Handler (throwM :: SomeAsyncException -> m w), Handler handler ]

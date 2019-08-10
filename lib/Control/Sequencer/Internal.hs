module Control.Sequencer.Internal where

import Control.Exception (Exception, SomeAsyncException, displayException)
import Control.Monad.Catch (MonadCatch, Handler(..), throwM, catches)
import Control.Applicative (Alternative)
import Data.Foldable (asum)

data SequencingFailure = SequencingFailure deriving Show
instance Exception SequencingFailure where displayException _ = "sequencing failure"

alternativeTranscode :: (Traversable ins, Alternative outs, Monad m)
                     => (m a -> m (outs b)) -> ins (m a) -> m (outs b)
alternativeTranscode f = fmap asum . sequence . fmap f

catchSynchronous :: forall e m a. (Exception e, MonadCatch m) => m a -> (e -> m a) -> m a
catchSynchronous action handler = action `catches`
    [ Handler (throwM :: SomeAsyncException -> m w), Handler handler ]

trySynchronous :: forall e m a. (Exception e, MonadCatch m) => m a -> m (Either e a)
trySynchronous x = fmap Right x `catchSynchronous` (return . Left)

module Control.Sequencer
    ( independent
    , insistent
    , redundant
    , trySynchronous
    , SequencingFailure(..)
    ) where

import Control.Exception (Exception, SomeException(..), ArithException)
import Control.Monad.Catch (MonadCatch, throwM)
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

insistent :: (MonadCatch m, MonadWriter (q SomeException) m, Alternative q)
          => Word -> m a -> m a
insistent n = redundant . genericReplicate n

redundant :: (MonadCatch m, MonadWriter (q SomeException) m, Foldable f, Alternative q)
          => f (m a) -> m a
redundant = foldr f (throwM SequencingFailure)
  where
    f x y = do
        r' <- trySynchronous @ArithException x
        case r' of
            Left e  -> do
                (tell . pure . SomeException) e
                y
            Right r -> return r

trySynchronous :: forall e m a. (Exception e, MonadCatch m) => m a -> m (Either e a)
trySynchronous x = fmap Right x `catchSynchronous` (return . Left)

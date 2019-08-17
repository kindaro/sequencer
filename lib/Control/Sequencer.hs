module Control.Sequencer
    ( independent
    , insistent
    , redundant
    , serialize
    , trySynchronous
    , SequencingFailure(..)
    ) where

import Control.Exception (Exception, SomeException)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Writer.Strict (MonadWriter, tell)
import Control.Applicative (Alternative, empty)
import Data.List (genericReplicate)
import Control.Exception (displayException)
import Control.Applicative ((<|>))

import Control.Sequencer.Internal

data SequencingFailure = SequencingFailure deriving Show
instance Exception SequencingFailure where displayException _ = "sequencing failure"

-- | Run all the actions independently from each other, collect the results and log the errors
-- with the supplied logger.
independent :: forall a m e ins outs.
                    (Exception e, MonadCatch m, Traversable ins, Alternative outs)
            => (e -> m ()) -> ins (m a) -> m (outs a)
independent logger = serialize f
    where f u = fmap pure u `catchSynchronous` \e -> logger e *> return empty

insistent :: (MonadCatch m, MonadWriter (q SomeException) m, Alternative q)
          => Word -> m a -> m a
insistent n = redundant . genericReplicate n

redundant :: (MonadCatch m, MonadWriter (q SomeException) m, Foldable f, Alternative q)
          => f (m a) -> m a
redundant = foldr1 f where f x y = x `catchSynchronous` (\e -> (tell . pure) e *> y)

trySynchronous :: forall e m a. (Exception e, MonadCatch m) => m a -> m (Either e a)
trySynchronous x = fmap Right x `catchSynchronous` (return . Left)

serialize :: (Traversable ins, Alternative outs, Monad m)
          => (m a -> m (outs b)) -> ins (m a) -> m (outs b)
serialize f = serialize' f (<|>)

serialize' :: (Traversable ins, Alternative outs, Monad m)
          => (m a -> m (outs b)) -> (outs b -> outs b -> outs b) -> ins (m a) -> m (outs b)
serialize' f g xs | null xs = return empty
                  | otherwise = (fmap (foldr1 g) . traverse f) xs

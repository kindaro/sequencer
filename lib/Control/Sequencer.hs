module Control.Sequencer
    ( independent
    , independent'
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

-- | Run all the actions independently from each other, collect the results in a list and log the
-- errors in a sequence.
independent :: forall a m ins. (MonadWriter (Seq SomeException) m, MonadCatch m, Traversable ins)
            => ins (m a) -> m [a]
independent = independent' (tell . pure)

-- | Run all the actions independently from each other, collect the results and log the errors
-- with the supplied logger.
independent' :: forall a m e ins outs.
                    (Exception e, MonadCatch m, Traversable ins, Alternative outs)
             => (e -> m ()) -> ins (m a) -> m (outs a)
independent' logger = alternativeTranscode f
    where f u = fmap pure u `catchSynchronous` \e -> logger e *> return empty

insistent :: (MonadCatch m, MonadWriter (q SomeException) m, Alternative q)
          => Word -> m a -> m a
insistent n = redundant . genericReplicate n

redundant :: (MonadCatch m, MonadWriter (q SomeException) m, Foldable f, Alternative q)
          => f (m a) -> m a
redundant = foldr1 f where f x y = x `catchSynchronous` (\e -> (tell . pure) e *> y)

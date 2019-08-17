{-# language UndecidableInstances #-}
{-# options_ghc -Wno-orphans -Wno-missing-methods #-}

module Assets where

import Prelude hiding (log)
import Test.QuickCheck
import Control.Exception
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Catch
import Control.Monad.Catch.Pure
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Data.Maybe
import Data.Foldable (toList)

import Instances.Utils.GenericArbitrary
import Instances.Control.Exception ()


-- * Foundational types.

-- A constraint for "useful" monads.
type MonadCheck m a = (MonadWriter (Seq SomeException) m, MonadState [Case a] m, MonadCatch m)

-- The specific monad "useful" for me.
type M a b = CatchT (StateT [Case a] (Writer (Seq SomeException))) b

-- Result type as returned by running an M action.
type RawRes a b = ((Either SomeException b, [Case a]), Seq SomeException)

-- The runner for M.
runM :: M a b -> [Case a] -> RawRes a b
runM action xs = runWriter . flip runStateT xs . runCatchT $ action

-- A nicer projection of a result of a run of M with effects captured.
data Res a b = Res
     { log :: [SomeException]
     , value :: Either SomeException b
     , remainder :: [Case a]
     } deriving (Show, Eq)

projectTarget :: RawRes a b -> Res a b
projectTarget ((value, remainder), log') = Res{..}
    where log = toList log'


-- * "Case" data type.

data Case a = ExpectedException ArithException
            | SuddenException ArrayException
            | AsynchronousException SomeAsyncException
            | Result a

instance {-# overlappable #-} Exception e => Eq e where
    e == e' = displayException e == displayException e'

deriving instance Eq a => Eq (Case a)
deriving instance Show a => Show (Case a)
deriving instance Generic (Case a)

instance Arbitrary a => Arbitrary (Case a) where
    arbitrary = genericArbitrary


-- * Example action.

exampleAction :: MonadCheck m a => m a
exampleAction = do
    u <- get
    case u of
        [ ] -> error "Action invoked while no items in state."
        (x: xs) -> do
            put xs
            case x of
                ExpectedException e     -> throwM e
                SuddenException e       -> throwM e
                AsynchronousException e -> throwM e
                Result r                -> return r

retractException :: forall a. SomeException -> Maybe (Case a)
retractException e = listToMaybe . catMaybes $
    [f ExpectedException, f SuddenException, f AsynchronousException] <*> [e]
  where f :: forall e. Exception e => (e -> Case a) -> SomeException -> Maybe (Case a)
        f g = fmap g . fromException

-- May return Nothing if an exception is not one that may be contained in a "case".
retractExampleAction :: forall a. Either SomeException a -> Maybe (Case a)
retractExampleAction = either retractException (Just . Result)


-- * Exception normalizer.

normalizeException :: forall e. Exception e => e -> SomeException
normalizeException = either id (error "Impossible code path: \
                            \ `runCatch . throwM` is always `Left`.") . runCatch . throwM

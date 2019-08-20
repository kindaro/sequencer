{-# language UndecidableInstances #-}
{-# options_ghc -Wno-orphans -Wno-missing-methods #-}

module Assets where

import Prelude hiding (log)
import Test.QuickCheck
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Catch
import Control.Monad.Catch.Pure
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Data.Maybe
import Data.Foldable (toList)
import Data.Either
import Control.Exception (ArithException, ArrayException, IOException, SomeAsyncException)

import Instances.Utils.GenericArbitrary
import Instances.Control.Exception ()


-- * Foundational types.

-- A constraint for "useful" monads.
type MonadCheck e m a = (MonadWriter (Seq e) m, MonadState [Either e a] m, MonadCatch m)

-- The specific monad "useful" for me.
type M e a b = CatchT (StateT [Either e a] (Writer (Seq e))) b

-- Result type as returned by running an M action.
type RawRes e a b = ((Either SomeException b, [Either e a]), Seq e)

-- The runner for M.
runM :: M e a b -> [Either e a] -> RawRes e a b
runM action xs = runWriter . flip runStateT xs . runCatchT $ action

-- A nicer projection of a result of a run of M with effects captured.
data Res e a b = Res
     { log :: [e]
     , value :: Either SomeException b
     , remainder :: [Either e a]
     } deriving (Show, Eq)

projectTarget :: RawRes e a b -> Res e a b
projectTarget ((value, remainder), log') = Res{..}
    where log = toList log'

instance {-# overlappable #-} Exception e => Eq e where
    (==) = compareExceptions


-- * Example action.

exampleAction :: forall e m a. (Exception e, MonadCheck e m a) => m a
exampleAction = do
    u <- get
    case u of
        [ ] -> error "Action invoked while no items in state."
        (x: xs) -> put xs >> (either (throwM @m @e) return) x


-- * Exception normalizer.

normalizeException :: forall e. Exception e => e -> SomeException
normalizeException = either id (error "Impossible code path: \
                            \ `runCatch . throwM` is always `Left`.") . runCatch . throwM

convertException :: forall e e'. (Exception e, Exception e') => e -> Maybe e'
convertException = fromException . toException

compareExceptions :: forall e e'. (Exception e, Exception e') => e -> e' -> Bool
compareExceptions e e' = displayException e == displayException e'

instance Arbitrary SomeException where
    arbitrary = oneof
        [ normalizeException <$> arbitrary @ArithException
        , normalizeException <$> arbitrary @ArrayException
        , normalizeException <$> arbitrary @IOException
        , normalizeException <$> arbitrary @SomeAsyncException
        ]

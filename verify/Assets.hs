{-# language UndecidableInstances #-}
{-# options_ghc -Wno-orphans -Wno-missing-methods #-}

module Assets where

import Prelude hiding (log)
import Data.Data
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
     { log :: [Case a]
     , value :: Either SomeException b
     , remainder :: [Case a]
     } deriving (Show, Eq)

projectTarget :: RawRes a b -> Res a b
projectTarget ((value, remainder), log') = Res{..}
    where log = (catMaybes . fmap retractException . toList) log'


-- * "Create value" class.

class CreateValue a v where createValue :: a -> v
instance CreateValue b x => CreateValue (a -> b) x where createValue f = createValue (f undefined)
instance {-# overlappable #-} CreateValue x x where createValue = id

is :: forall a v. (CreateValue a v, Data v) => v -> a -> Bool
is x c = toConstr x == toConstr (createValue c :: v)


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
deriving instance Typeable a => Typeable (Case a)

instance Typeable a => Data (Case a) where
    toConstr (ExpectedException _) = con_ExpectedException
    toConstr (SuddenException _) = con_SuddenException
    toConstr (AsynchronousException _) = con_AsynchronousException
    toConstr (Result _) = con_Result
    dataTypeOf _ = ty_Case

con_ExpectedException, con_SuddenException, con_AsynchronousException, con_Result :: Constr
con_ExpectedException = mkConstr ty_Case "ExpectedException" [] Prefix
con_SuddenException = mkConstr ty_Case "SuddenException" [] Prefix
con_AsynchronousException = mkConstr ty_Case "AsynchronousException" [] Prefix
con_Result = mkConstr ty_Case "Result" [] Prefix

ty_Case :: DataType
ty_Case = mkDataType "Main.Case"
    [ con_ExpectedException
    , con_SuddenException
    , con_AsynchronousException
    , con_Result ]

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

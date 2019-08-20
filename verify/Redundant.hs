module Redundant where

import Prelude hiding (log)
import qualified Data.List as List
import Data.Maybe
import Control.Monad.Catch
import Control.Monad.Catch.Pure
import Control.Monad.Writer (tell)
import Data.Bifunctor (bimap)
import Data.Either
import Control.Exception (ArithException, ArrayException)
import Data.Hashable

import Control.Sequencer (redundant, SequencingFailure(..))
import Control.Sequencer.Internal (catchesSynchronous)
import Assets


prop_redundant_all :: [Either SomeException Integer] -> Bool
prop_redundant_all xs = left == right
  where left = projectSource xs
        right = projectTarget (runM action xs)
        action = redundant exampleHandlers (tell . pure . normalizeException) (replicate (length xs) exampleAction)

exampleHandlers :: forall m. MonadThrow m => [Handler m SomeException]
exampleHandlers =
    [ Handler \e -> if (hash . show @ArithException) e `mod` 10 /= 0
                       then (return . normalizeException) e
                       else throwM e
    , Handler \e -> if (hash . show @ArrayException) e `mod` 10 /= 0
                       then (return . normalizeException) e
                       else throwM e
    ]

projectSource :: forall e a. Exception e => [Either e a] -> Res e a a
projectSource xs = let value     = predictValue     xs
                       log       = predictLog       xs
                       remainder = predictRemainder xs
                   in Res{..}

isTerminal :: forall e a b. (Exception e) => [Handler Catch b] -> Either e a -> Bool
isTerminal _ (Right _) = True
isTerminal hs (Left e) = not (e `isCaughtBy` hs)

predictValue :: forall e a. (Exception e) => [Either e a] -> Either SomeException a
predictValue xs = fromMaybe (Left (normalizeException SequencingFailure))
                $ List.find (isTerminal exampleHandlers) xs
                    >>= return . bimap (normalizeException) id

predictLog :: forall e a. Exception e => [Either e a] -> [e]
predictLog = fmap (either id (error "Impossible code path: \
        \`isTerminal` always matches `Right`.")) . takeWhile (not . (isTerminal exampleHandlers))

predictRemainder :: forall e a. Exception e => [Either e a] -> [Either e a]
predictRemainder = drop 1 . dropWhile (not . (isTerminal exampleHandlers))

isCaughtBy :: forall e a. Exception e => e -> [Handler Catch a] -> Bool
isCaughtBy e hs = isRight . runCatch $ throwM (e :: e) `catchesSynchronous` hs

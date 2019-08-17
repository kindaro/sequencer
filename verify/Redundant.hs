module Redundant where

import Prelude hiding (log)
import qualified Data.List as List
import Control.Exception
import Data.Foldable (asum)
import Data.Maybe

import Control.Sequencer (redundant, SequencingFailure(..))
import Assets


prop_redundant_all :: [Case Integer] -> Bool
prop_redundant_all xs = left == right
  where left = projectSource xs
        right = projectTarget run
        run = runM action xs
        action = redundant (replicate (length xs) exampleAction)


projectSource :: forall a. [Case a] -> Res a a
projectSource xs = let value     = predictValue     xs
                       log       = predictLog       xs
                       remainder = predictRemainder xs
                   in Res{..}

predictValue :: forall a. [Case a] -> Either SomeException a
predictValue cases = fromJust . asum . (++ [failure]) . fmap caseToOutcome $ cases
    where failure = (Just . Left . SomeException) SequencingFailure

predictLog :: forall a. [Case a] -> [Case a]
predictLog = takeWhile (isNothing . caseToOutcome)

predictRemainder :: forall a. [Case a] -> [Case a]
predictRemainder xs = fromMaybe [ ] . fmap (\n -> cutFrom n xs)
                                    . List.findIndex isJust . fmap caseToOutcome $ xs
    where cutFrom n = tail . snd . splitAt n

caseToOutcome :: Case a -> Maybe (Either SomeException a)
caseToOutcome (AsynchronousException e) = (Just . Left . SomeException) e
caseToOutcome (ExpectedException _) = Nothing
caseToOutcome (SuddenException e) = (Just . Left . SomeException) e
caseToOutcome (Result r) = Just (Right r)

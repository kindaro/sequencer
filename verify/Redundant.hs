module Redundant where

import Prelude hiding (log)
import Data.Typeable
import qualified Data.List as List
import Control.Exception
import Data.Function (on)
import Data.Maybe

import Control.Sequencer (redundant, SequencingFailure(..))
import Assets


prop_redundant_all :: [Case Integer] -> Bool
prop_redundant_all xs = left == right
  where left = projectSource xs
        right = projectTarget run
        run = runM action xs
        action = redundant (replicate (length xs) exampleAction)


projectSource :: forall a b. Typeable a => [Case a] -> Res a b
projectSource xs = case predictOutcome xs of
    Left e -> Res { log = xs, value = Left e, remainder = [ ] }
    Right (n, value') -> case List.genericSplitAt n xs of
                            (log, (_: remainder)) -> Res { value = Right value', .. }
                            _ -> error "Impossible code path"

predictOutcome :: forall a b. Typeable a => [Case a] -> Either SomeException (Word, b)
predictOutcome cases = case fmap (List.minimumBy ((compare :: Word -> Word -> Ordering) `on` fst)) . mconcat . (fmap . fmap) (pure @[])
                     $ ($ (zip [0 :: Word ..] cases)) <$> loci of
                            Just (n, r') -> let r = (fromJust (error "") . caseToOutcome) r'
                                            in  Right (n, r)
                            Nothing  -> (Left . SomeException) SequencingFailure

    where locus :: forall u. (u -> Case a) -> [(Word, Case a)] -> Maybe (Word, Case a)
          locus f = List.find (flip is f . snd)
          loci = [locus SuddenException, locus AsynchronousException, locus Result]

caseToOutcome :: Case a -> Maybe (Either SomeException a)
caseToOutcome (AsynchronousException e) = (Just . Left . SomeException) e
caseToOutcome (ExpectedException _) = Nothing
caseToOutcome (SuddenException e) = (Just . Left . SomeException) e
caseToOutcome (Result r) = Just (Right r)

module Economics.Utility.VNM.Helpers where

import Prelude

import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty.Indexed as Indexed
import Data.Tuple (Tuple(..))
import Data.Unfoldable as Unfoldable
import Partial.Unsafe (unsafeCrashWith)

unsafeFromJustBecause :: forall a. String -> Maybe a -> a
unsafeFromJustBecause _ (Just a) = a
unsafeFromJustBecause str Nothing = unsafeCrashWith str

pairCombos :: forall a. List a -> List (Tuple a a)
pairCombos l = do
  ls <- tails l
  case ls of
    Nil -> Nil
    Cons x ys -> do
      y <- ys
      pure $ Tuple x y

nonEmptyMap ::
     forall k v.
     Ord k
  => Map k v
  -> Maybe (Indexed.NonEmpty Map k v)
nonEmptyMap m =
  (\l -> (Tuple l.key l.value) Indexed.:| (l.key `Map.delete` m)) <$> Map.findMin m

tails :: forall a. List a -> List (List a)
tails Nil = Unfoldable.singleton Nil
tails as'@(Cons a as) = as' : tails as

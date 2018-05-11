module Economics.Utility.VNM.Function where

import Prelude

import Data.Either (Either(Right, Left))
import Data.Foldable (length)
import Data.Generic (class Generic)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, fromNonEmpty)
import Data.NonEmpty.Indexed as Indexed
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Math.Interval ((<=!), (>=!))
import Math.Interval (singleton) as Interval
import Math.Interval.Bound (Finite(MkFinite), PosInf(MkPosInf), lower, upper)
import Math.Interval.Internal (NonEmpty(MkNonEmpty)) as Interval
import Math.Interval.Openness (Openness(..))
import Partial.Unsafe (unsafeCrashWith)

import Economics.Utility.Ratio (Pair(..), Ratio(..))
import Economics.Utility.VNM.Helpers (nonEmptyMap, pairCombos, unsafeFromJustBecause)


newtype UtilityFn a n = MkUtilityFn (Map (Pair a) (Interval.NonEmpty n))
derive newtype instance showUtilityFn :: (Generic a, Generic n) => Show (UtilityFn a n)


goods :: forall a n. Ord a => UtilityFn a n -> Set a
goods (MkUtilityFn fn) =
  Set.fromFoldable <<<
  (_ >>= \(MkPair {quote, base}) -> quote : base : Nil) <<< Map.keys $
  fn

pairs :: forall a n. Ord a => UtilityFn a n -> Set (Pair a)
pairs (MkUtilityFn m) = Set.fromFoldable $ Map.keys m

unmake ::
     forall n a.
     Ord a
  => Ord n
  => UtilityFn a n -> Indexed.NonEmpty Map (Pair a) (Interval.NonEmpty n)
unmake (MkUtilityFn m) =
  unsafeFromJustBecause "We should never have an empty set of pairs" <<<
  nonEmptyMap $
  m


-- | Must pass in a set with at least two elements
goodsToInitialFn ::
     forall a n.
     Ord a
  => Ord (Pair a)
  => Ord n
  => Semiring n
  => NonEmpty Set a
  -> Either Unit (UtilityFn a n)
goodsToInitialFn set
  | length set == 1 = Left unit
  | otherwise =
    Right <<<
    MkUtilityFn <<<
    Map.fromFoldable <<<
    map ((flip Tuple positive) <<< mkPair) <<<
    pairCombos <<< Set.toUnfoldable <<< fromNonEmpty Set.insert $
    set
      where
        positive
          = Interval.MkNonEmpty
          { lower: lower <<< Right $ MkFinite { bound: zero, openness: Open }
          , upper: upper <<< Right $ MkPosInf
          }
        mkPair (Tuple base quote) = MkPair {base, quote}


update ::
     forall a n. Ord a
  => Pair a
  -> (Interval.NonEmpty n -> Interval.NonEmpty n)
  -> UtilityFn a n
  -> UtilityFn a n
update key f (MkUtilityFn m) = MkUtilityFn $ Map.update (Just <<< f) key m

-- | Prune utility function so all remaining ratios involve chosen good
prune :: forall a n. Ord a => a -> UtilityFn a n -> UtilityFn a n
prune a (MkUtilityFn m) = MkUtilityFn $ Map.filterKeys (contains a) m
  where
    contains a' (MkPair { quote, base }) = quote == a' || base == a'

byGood ::
     forall a n.
     Ord a
  => Ord n
  => UtilityFn a n
  -> Map a (Set (Ratio a (Interval.NonEmpty n)))
byGood (MkUtilityFn m) = Map.fromFoldableWith (<>) do
  Tuple (pair@MkPair {quote, base}) relativeValue <- Map.toUnfoldable $ m
  let ratio = MkRatio { pair, relativeValue }
  (Tuple quote (Set.singleton ratio) : Tuple base (Set.singleton ratio) : Nil)


best :: forall a n. Ord a => Ord n => Semiring n => UtilityFn a n -> Maybe a
best =
  extractOnly <<<
  Map.filter (_ == Set.singleton true) <<<
  Map.mapWithKey (\good ratios -> Set.map ((_ == Just good) <<< better) ratios) <<<
  byGood
  where
    extractOnly m
      | length m > 1 =
        unsafeCrashWith "There should only ever be one 'best' good"
      | otherwise = _ . key <$> Map.findMin m

better :: forall a n. Ord n => Semiring n => Ratio a (Interval.NonEmpty n) -> Maybe a
better (MkRatio { pair: (MkPair { base, quote }), relativeValue })
  | relativeValue <=! Interval.singleton one = Just quote
  | relativeValue >=! Interval.singleton one = Just base
  | otherwise = Nothing

worse :: forall a n. Ord n => Semiring n => Ratio a (Interval.NonEmpty n) -> Maybe a
worse (MkRatio { pair: (MkPair { base, quote }), relativeValue })
  | relativeValue <=! Interval.singleton one = Just base
  | relativeValue >=! Interval.singleton one = Just quote
  | otherwise = Nothing

module Economics.Utility.VNM where

import Prelude hiding (bottom,top)

import Data.Foldable (class Foldable, maximumBy)
import Data.Function (on)
import Data.List (List)
import Data.Map as Map
import Data.NonEmpty.Indexed as Indexed
import Data.Tuple (Tuple(Tuple), snd)
import Math (sqrt)
import Math.Interval (boundAbove, boundBelow, normalizedWidth)
import Math.Interval as Interval
import Math.Interval.Bound (Bound(..))
import Math.Interval.Internal (Interval(..))
import Math.Interval.Openness (Openness(..))
import Partial.Unsafe (unsafeCrashWith)

import Economics.Utility.Ratio (Ratio(MkRatio))
import Economics.Utility.VNM.Function (UtilityFn, unmake)
import Economics.Utility.VNM.Function as Function
import Economics.Utility.VNM.Helpers (unsafeFromJustBecause)

top :: Number
top = 1e4

smallest :: Number
smallest = 1e-4

bottom :: Number
bottom = 1e-4

geometricMean :: Number -> Number -> Number
geometricMean l r = sqrt l * sqrt r

-- | Geometric mean of two numbers with some business logic for handling edge cases
geometricMidpointish :: Bound Number -> Bound Number -> Number
geometricMidpointish NegInf PosInf = 0.0
geometricMidpointish NegInf (Finite { bound })
  | bound > -1.0 = -1.0
  | otherwise = geometricMean (-bound) (-bottom)
geometricMidpointish PosInf r = geometricMidpointish r PosInf
geometricMidpointish l NegInf = geometricMidpointish NegInf l
geometricMidpointish (Finite { bound }) PosInf
  | bound < 1.0 = 1.0
  | otherwise = geometricMean bound top
geometricMidpointish (Finite { bound: 0.0, openness }) (Finite r) =
  geometricMidpointish (Finite { bound: smallest, openness }) (Finite r)
geometricMidpointish (Finite l) (Finite r)
  | l.bound < 0.0 && r.bound < 0.0 = geometricMean (-l.bound) (-r.bound)
  | otherwise = geometricMean l.bound r.bound

widest ::
     forall a f n.
     EuclideanRing n
  => Ord a
  => Ord n
  => Foldable f
  => Ring n
  => f (Tuple a (Interval n))
  -> Tuple a (Interval n)
widest =
  unsafeFromJustBecause "`NonEmpty`" <<<
  maximumBy (compare `on` (normalizedWidth <<< snd))

pickNextLottery ::
     forall a.
     Ord a
  => UtilityFn a Number
  -> Ratio a Number
pickNextLottery fn =
 case widest <<< asList <<< Map.toUnfoldable <<< Indexed.fromNonEmpty Map.insert <<< unmake $ fn of
    Tuple _ Empty -> unsafeCrashWith "Our intervals should never be empty"
    Tuple pair (NonEmpty {lower, upper}) ->
      MkRatio { pair, relativeValue : geometricMidpointish lower upper }
 where
   asList :: forall b. List b -> List b
   asList = id

refine ::
     forall a n.
     Ord a
  => Ord n
  => Ratio a n
  -> Ordering
  -> UtilityFn a n
  -> UtilityFn a n
refine (MkRatio ratio) EQ =
  Function.update ratio.pair (const $ Interval.singleton ratio.relativeValue)
refine (MkRatio ratio) GT =
  Function.update
    ratio.pair
    (unsafeFromJustBecause "Midpoint guaranteed within interval" <<<
     boundBelow ratio.relativeValue Open)
refine (MkRatio ratio) LT =
  Function.update
    ratio.pair
    (unsafeFromJustBecause "Midpoint guaranteed within interval" <<<
     boundAbove ratio.relativeValue Open)

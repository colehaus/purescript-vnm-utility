module Economics.Utility.VNM where

import Prelude hiding (bottom,top)

import Data.Either (Either(..))
import Data.Either.Nested (either3)
import Data.Foldable (class Foldable, maximumBy)
import Data.Function (on)
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty.Indexed as Indexed
import Data.Tuple (Tuple(Tuple), snd)
import Economics.Utility.Ratio (Ratio(MkRatio))
import Economics.Utility.VNM.Function (UtilityFn, unmake)
import Economics.Utility.VNM.Function as Function
import Economics.Utility.VNM.Helpers (unsafeFromJustBecause)
import Math (sqrt)
import Math.Interval (boundAbove, boundBelow, normalizedWidth)
import Math.Interval (singleton) as Interval
import Math.Interval.Bound (Finite(MkFinite), Lower, Upper, lower, raw, upper)
import Math.Interval.Internal (Interval(..))
import Math.Interval.Internal (NonEmpty(MkNonEmpty)) as Interval
import Math.Interval.Openness (Openness(..))


top :: Number
top = 1e4

smallest :: Number
smallest = 1e-4

bottom :: Number
bottom = 1e-4

geometricMean :: Number -> Number -> Number
geometricMean l r = sqrt l * sqrt r

-- | Geometric mean of two numbers with some business logic for handling edge cases
geometricMidpointish :: Lower Number -> Upper Number -> Number
geometricMidpointish l u =
  either3
    (\_ ->
       either3
         absurd
         (\{bound} ->
            if bound > -1.0
              then -1.0
              else geometricMean (-bound) (-bottom))
         (\_ -> 0.0) <<<
       raw $
       u)
    (\lf ->
       either3
         absurd
         (f (MkFinite lf) <<< MkFinite)
         (\_ ->
            if lf.bound < 1.0
              then 1.0
              else geometricMean lf.bound top) <<<
       raw $
       u)
    absurd <<<
  raw $
  l
  where
    f (MkFinite {bound: 0.0, openness}) (MkFinite r) =
      geometricMidpointish
        (lower <<< Right $ MkFinite { bound: smallest, openness })
        (upper <<< Left $ MkFinite r)
    f (MkFinite l) (MkFinite r)
      | l.bound < 0.0 && r.bound < 0.0 = geometricMean (-l.bound) (-r.bound)
      | otherwise = geometricMean l.bound r.bound

widest ::
     forall a f n.
     EuclideanRing n
  => Ord a
  => Ord n
  => Foldable f
  => Ring n
  => f (Tuple a (Interval.NonEmpty n))
  -> Tuple a (Interval.NonEmpty n)
widest =
  unsafeFromJustBecause "`NonEmpty`" <<<
  maximumBy (compare `on` (normalizedWidth <<< MkInterval <<< Right <<< snd))

pickNextLottery ::
     forall a.
     Ord a
  => UtilityFn a Number
  -> Ratio a Number
pickNextLottery fn =
 case widest <<< asList <<< Map.toUnfoldable <<< Indexed.fromNonEmpty Map.insert <<< unmake $ fn of
    Tuple pair (Interval.MkNonEmpty {lower, upper}) ->
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
  Function.update ratio .
  pair
    (unsafeFromJustBecause "Midpoint guaranteed within interval" <<<
     (nonEmpty <=< boundBelow ratio . relativeValue Open))
refine (MkRatio ratio) LT =
  Function.update ratio .
  pair
    (unsafeFromJustBecause "Midpoint guaranteed within interval" <<<
     (nonEmpty <=< boundAbove ratio . relativeValue Open))

nonEmpty :: forall n. Interval n -> Maybe (Interval.NonEmpty n)
nonEmpty (MkInterval (Right r)) = Just r
nonEmpty _ = Nothing

module Economics.Utility.Ratio where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)


newtype Pair a = MkPair { base :: a, quote :: a }

derive instance newtypePair :: Newtype (Pair a) _
derive instance genericPair :: Generic (Pair a) _
derive instance eqPair :: Eq a => Eq (Pair a)
instance showPair :: Show a => Show (Pair a) where show = genericShow

-- | WARNING: Not a semantic instance. Just so we can put it inside a map.
derive instance ordPair :: Ord a => Ord (Pair a)


newtype Ratio a n = MkRatio { pair :: Pair a, relativeValue :: n }

derive instance newtypeRatio :: Newtype (Ratio a n) _
derive instance genericRatio :: Generic (Ratio a n) _
derive instance eqRatio :: (Eq a, Eq n) => Eq (Ratio a n)
instance showRatio :: (Show a, Show n) => Show (Ratio a n) where show = genericShow

-- | WARNING: Not a semantic instance. Just so we can put it inside a map.
derive instance ordRatio :: (Ord a, Ord n) => Ord (Ratio a n)

base :: forall a n. Ratio a n -> a
base (MkRatio { pair: MkPair { base } }) = base

quote :: forall a n. Ratio a n -> a
quote (MkRatio { pair: MkPair { quote } }) = quote

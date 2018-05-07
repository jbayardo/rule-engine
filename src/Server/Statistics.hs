{-# LANGUAGE DeriveGeneric #-}

module Server.Statistics where

import qualified Engine.Rule as R
import qualified Engine.Tree as T

import Control.Exception (SomeException)
import qualified Control.Lens as Lens
import qualified Data.Functor.Foldable as F
import qualified Data.Map.Strict as M
import qualified Data.Monoid as Monoid
import Generics.Deriving.Base (Generic)
import Generics.Deriving.Monoid

fullBottomUpMerge :: (Monoid m) => T.Tree m -> m
fullBottomUpMerge = F.cata go
  where
    go T.FailureF = mempty
    go T.NodeF {T.valueF = value, T.leftF = left, T.rightF = right} =
      left `mappend` value `mappend` right

bottomUpMerge :: (Monoid m) => T.Tree m -> T.Tree m
bottomUpMerge = F.cata go
  where
    go T.FailureF = T.Failure
    go T.NodeF {T.valueF = value, T.leftF = left, T.rightF = right} =
      T.Node
        { T.value =
            T.from mempty left `mappend` value `mappend` T.from mempty right
        , T.left = left
        , T.right = right
        }

topDownMerge :: (Monoid m) => T.Tree m -> T.Tree m
topDownMerge = F.ana go
  where
    go T.Failure = T.FailureF
    go T.Node {T.value = value, T.left = left, T.right = right} =
      T.NodeF
        { T.valueF = value
        , T.leftF = T.transform (mappend value) left
        , T.rightF = T.transform (mappend value) right
        }

zipMerge :: (Monoid m) => [T.Tree m] -> Maybe (T.Tree m)
zipMerge [] = Nothing
zipMerge trees = Just $ F.ana go trees
  where
    go trees =
      let trees' = filter T.isNode trees
       in if null trees'
            then T.FailureF
            else T.NodeF
                   { T.valueF = foldl1 mappend $ T.value <$> trees'
                   , T.leftF = T.left <$> trees'
                   , T.rightF = T.right <$> trees'
                   }

-- This keeps counts of different keys as they appeared
newtype BucketCount k = BucketCount
  { getBuckets :: M.Map k Integer
  } deriving (Show)

increase :: (Ord k) => k -> BucketCount k -> BucketCount k
increase key (BucketCount b) = BucketCount $ M.insertWith (+) key 1 b

instance (Ord k) => Monoid (BucketCount k) where
  mempty = BucketCount M.empty
  (BucketCount m1) `mappend` (BucketCount m2) =
    BucketCount $ M.unionWith (+) m1 m2

data Statistics d = Statistics
  { _calls :: !(Monoid.Sum Integer)
  , _numErrors :: !(Monoid.Sum Integer)
  , _seen :: !(BucketCount d)
  , _moves :: !(BucketCount T.Movement)
  , _errors :: ![SomeException]
  } deriving (Show, Generic)

Lens.makeLenses ''Statistics

instance (Ord d) => Monoid (Statistics d) where
  mempty = memptydefault
  mappend = mappenddefault

-- TODO: remove Ord constraint
class IntoStatistics a where
  intoStatistic :: (Ord d) => a d -> Statistics d

instance IntoStatistics R.Execution where
  intoStatistic execution = R.destruct e d m execution basic
    where
      basic = Lens.over calls (+ 1) mempty
      e exception = Lens.over numErrors (+ 1) . Lens.over errors (exception :)
      d = Lens.over seen . increase
      m = Lens.over moves . increase

instance IntoStatistics Statistics where
  intoStatistic = id

translate :: (IntoStatistics a, Ord d) => T.Tree (a d) -> T.Tree (Statistics d)
translate = fmap intoStatistic

{-# SPECIALISE translate ::
                 (Ord d) => T.Tree (R.Execution d) -> T.Tree (Statistics d) #-}

{-# SPECIALISE translate ::
                 (Ord d) => T.Tree (Statistics d) -> T.Tree (Statistics d) #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Engine.Tree where

import qualified Data.Functor.Foldable.TH as R

data Tree a
  = Node { value :: !a
         , left :: Tree a
         , right :: Tree a }
  | Failure
  deriving (Functor, Foldable, Traversable, Show)

R.makeBaseFunctor ''Tree

from :: a -> Tree a -> a
from d Failure = d
from _ Node {value} = value

isFailure :: Tree a -> Bool
isFailure Failure = True
isFailure _ = False

isNode :: Tree a -> Bool
isNode = not . isFailure

circular :: a -> Tree a
circular a = Node a (circular a) (circular a)

deadend :: a -> Tree a
deadend a = Node a Failure Failure

transform :: (a -> a) -> Tree a -> Tree a
transform f Failure = Failure
transform f node@Node {value} = node {value = f value}

data Movement
  = L
  | R
  deriving (Show, Eq, Ord, Enum)

branch :: Movement -> Tree a -> Tree a
branch L = left
branch R = right

conditional :: a -> Tree a -> Movement -> Tree a
conditional x branch L = Node {value = x, left = branch, right = Failure}
conditional x branch R = Node {value = x, left = Failure, right = branch}

type Select a = Either (a, Tree a) (a, Tree a)

type Thread a = [Select a]

type Zipper a = (Tree a, Thread a)

into :: Tree a -> Zipper a
into = (, [])

recover :: Zipper a -> Tree a
recover = fst

hasParent :: Zipper a -> Bool
hasParent (_, []) = False
hasParent _ = True

root :: Zipper a -> Zipper a
root = until (not . hasParent) up

up :: Zipper a -> Zipper a
up (node, crumb:bs) =
  (, bs) $
  case crumb of
    Left (value, right) -> Node {value = value, left = node, right = right}
    Right (value, left) -> Node {value = value, left = left, right = node}
up zipper = zipper

down :: Zipper a -> Movement -> Zipper a
down (Node {value, left, right}, breadcrumbs) move =
  case move of
    L -> (left, Left (value, right) : breadcrumbs)
    R -> (right, Right (value, left) : breadcrumbs)
down zipper _ = zipper

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (node@Node {value}, bs) = (, bs) node {value = f value}
modify _ node = node

seek :: (Foldable f) => Zipper a -> f Movement -> Zipper a
seek = foldl down

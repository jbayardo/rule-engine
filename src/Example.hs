module Main where

import qualified Control.Lens as Lens
import Control.Monad (replicateM)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch, throwM)
import Data.Monoid
import Engine
import Engine.Rule
import Server
import qualified Engine.Tree as Tree
import System.Random

data Decision
  = Good
  | Bad
  | Unknown
  deriving (Show, Eq, Ord, Enum)

good :: (Monad m) => Tree (Rule m Decision)
good = Tree.deadend $ always Good

bad :: (Monad m) => Tree (Rule m Decision)
bad = Tree.deadend $ always Bad

unknown :: (Monad m) => Tree (Rule m Decision)
unknown = Tree.deadend $ always Unknown

coin :: Rule IO Decision
coin = do
  toss <- randomRIO (0, 4 :: Int)
  case toss of
    0 -> decide L
    1 -> decide R
    2 -> fail "Damn!"
    3 -> decide Good
    4 -> decide Bad

exampleTree :: Tree (Rule IO Decision)
exampleTree = circular coin

easy :: Int -> Tree Int
easy 0 = deadend 0
easy n = Node n (easy (n - 1)) (easy (n - 1))

easy' = fmap Sum . easy

executeWithStats ::
     (Ord d, MonadCatch m) => Tree (Rule m d) -> Int -> m (Tree (Statistics d))
executeWithStats tree times = do
  summaries <- replicateM times $ executeWithSummary tree
  let execution = (Lens.^. details) <$> summaries
  let statistics = translate <$> execution
  let (Just t) = zipMerge statistics
  return t

main :: IO ()
main = print "Hullo"

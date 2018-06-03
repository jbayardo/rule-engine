{-# LANGUAGE FlexibleContexts #-}

module Engine.Execute where

import qualified Engine.Rule as R
import qualified Engine.Tree as T

import Control.Exception
import qualified Control.Lens as Lens
import Control.Monad ((<=<), liftM, mapM, when)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch, throwM)
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as StateT
import qualified Data.Functor.Foldable as F

data Callbacks m d = Callbacks
  -- This callback is called every time a rule is run, and given the results of
  -- the run. This allows one to process those results into something else.
  { run :: R.Execution d -> m ()
  -- This callback is called if we detect an unrecoverable error while 
  -- navigating the tree.
  , unrecoverable :: m ()
  }

anaM ::
     (Monad m, F.Corecursive b, Traversable (F.Base b))
  => (a -> m (F.Base b a))
  -> a
  -> m b
anaM coalg = recurse
  where
    recurse = (return . F.embed) <=< mapM recurse <=< coalg

execute ::
     (MonadCatch m)
  => Callbacks m d
  -> T.Tree (R.Rule m d)
  -> m (T.Tree (R.Execution d))
execute Callbacks {unrecoverable, run} = anaM go
  where
    go T.Failure = unrecoverable >> return T.FailureF
    go tree@T.Node {T.value = rule} = do
      !execution <- R.apply rule
      run execution
      let endpoint = const $ T.deadendF execution T.Failure
      return $
        R.destruct
          endpoint
          endpoint
          (\move -> T.conditionalF execution (T.branch move tree) move)
          execution

execute' :: (MonadCatch m) => T.Tree (R.Rule m d) -> m (T.Tree (R.Execution d))
execute' =
  execute Callbacks {run = const $ return (), unrecoverable = return ()}

-- This exception is only produced when the tree somehow ends in an invalid
-- execution state.
data InvalidTreeException =
  InvalidTreeException
  deriving (Show)

instance Exception InvalidTreeException

data Summary d = Summary
  -- The trace is precisely which directions we took up until the moment when
  -- execution finished.
  { _trace :: ![T.Movement]
  -- Either we managed to make a decision, or there was a failure while 
  -- attempting to do so. If we failed, the error is stored in here.
  , _decision :: !(Either SomeException d)
  -- Contains the original execution tree, just in case it is needed to do any
  -- further processing.
  , _details :: T.Tree (R.Execution d)
  } deriving (Show)

Lens.makeLenses ''Summary

executeWithSummary ::
     forall m d. (MonadCatch m)
  => T.Tree (R.Rule m d)
  -> m (Summary d)
executeWithSummary tree
  -- We start with this Summary
 = do
  let initial =
        Summary
          { _trace = []
          , _decision = Left $ toException InvalidTreeException
          , _details = T.Failure
          }
  -- These are the callbacks that are going to update the initial summary that
  -- we are using as state
  let callbacks =
        Callbacks {run = updateRun, unrecoverable = updateUnrecoverable}
  -- We need to lift the monadic actions inside the tree to work within the
  -- StateT monad transformer that we have defined. Thankfully, lazyness will
  -- prevent us from evaluating the lift unless we actually need the node
  let tree' = Trans.lift <$> tree
  -- Effectively compute the tree
  (execution, summary) <- StateT.runStateT (execute callbacks tree') initial
  -- Return the updated summary with the full execution tree
  return summary {_details = execution}
  where
    updateRun :: R.Execution d -> StateT.StateT (Summary d) m ()
    updateRun =
      R.destruct
        (Lens.assign decision . Left)
        (Lens.assign decision . Right)
        (\m -> Lens.modifying trace (m :))
    updateUnrecoverable =
      Lens.assign decision . Left $ toException InvalidTreeException

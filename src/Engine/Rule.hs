-- These are required to be able to define the IntoOutcome type class
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Engine.Rule
  ( Outcome(..)
  , decide
  , Rule
  , rule
  , always
  , Execution(..)
  , failed
  , outcome
  , apply
  , destruct
  ) where

import Engine.Tree (Movement)

import Control.Exception hiding (catch)
import qualified Control.Lens as Lens
import Control.Monad.Catch (MonadCatch, MonadThrow, catch, throwM)
import qualified Data.Either as E

type Outcome d = Either d Movement

-- This typeclass exists to help with conversion from any type into an outcome
-- of some decision.
class IntoOutcome t d where
  intoOutcome :: t -> Outcome d

instance IntoOutcome (Outcome d) d where
  intoOutcome = id

instance IntoOutcome d d where
  intoOutcome = Left

instance IntoOutcome Movement d where
  intoOutcome = Right

decide :: (IntoOutcome t d, Monad m) => t -> m (Outcome d)
decide = return . intoOutcome

-- Every Rule executes within some Monad m and produces a value of type d or
-- tells some movement to apply to the tree. Although this is a type alias for
-- now, it may change in the future, and hence we export the below functions
-- to work with rules.
type Rule m d = m (Outcome d)

rule :: (IntoOutcome t d, Monad m) => m t -> Rule m d
rule = (>>= decide)

always :: (IntoOutcome t d, Monad m) => t -> Rule m d
always = decide

newtype Execution d = Execution
  { _result :: Either SomeException (Outcome d)
  -- TODO: add rule performance measuring. Basically just the amount of memory
  -- used and time taken to reach a decision. See `clock` package.
  } deriving (Show)

Lens.makeLenses ''Execution

failed :: Execution d -> Bool
failed = E.isLeft . (Lens.^. result)

outcome :: Execution d -> Outcome d
outcome = E.fromRight undefined . (Lens.^. result)

apply :: (MonadCatch m) => Rule m d -> m (Execution d)
apply rule =
  do outcome <- rule
     return Execution {_result = Right outcome}
     `catch` \exception -> return Execution {_result = Left exception}

destruct ::
     (SomeException -> a) -> (d -> a) -> (Movement -> a) -> Execution d -> a
destruct e d m execution =
  case execution Lens.^. result of
    Left exception -> e exception
    Right outcome ->
      case outcome of
        Left decision -> d decision
        Right move -> m move

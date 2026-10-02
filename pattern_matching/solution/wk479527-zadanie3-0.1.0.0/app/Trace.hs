module Trace
  ( ReduceState (..),
    ReduceM,
    record,
    consumeFuel,
    putCounter,           -- Fresh name counter.
  )
where

import Context
import Control.Monad.State.Strict
import SnocList
import Syntax

data ReduceState = ReduceState
  { stDefs :: DefMap,
    stCounter :: Int,
    stFuel :: Int,
    stHistory :: SnocList Step
  }

type ReduceM a = State ReduceState a


-- Records a focused expression, avoiding duplicate consecutive steps.
record :: Expr -> Path -> ReduceM ()
record expr path = do
  st <- get
  let oldHistory = stHistory st
      newStep = Step expr path
  case lastSnoc oldHistory of
    Just oldStep
      | oldStep == newStep ->
          return ()
    _ ->
      put st {stHistory = snoc oldHistory newStep}

consumeFuel :: ReduceM Bool
consumeFuel = do
  st <- get
  if stFuel st <= 0
    then return False
    else do
      put st {stFuel = stFuel st - 1}
      return True

putCounter :: Int -> ReduceM ()
putCounter i =
  modify (\st -> st {stCounter = i})

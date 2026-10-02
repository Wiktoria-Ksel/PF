module ReducerTrace
  ( reduceTrace,
    rstepTrace,
  )
where

import Context
import Control.Monad.State.Strict
import Data.List (foldl')
import qualified Data.Map as Map
import Substitution
import Syntax
import Trace

-- Result of trying one equation; 
-- failure may still update the expression.
data MatchAttempt
  = MatchSucceeded Expr [Expr] Expr
  | MatchFailed Expr

-- Performs one traced reduction step on the whole expression.
rstepTrace :: Expr -> ReduceM (Maybe Expr)
rstepTrace expr =
  rstepTraceAt expr [] expr

-- Tries to reduce the current subexpression and records 
-- the focused position inside the whole expression.
rstepTraceAt :: Expr -> Path -> Expr -> ReduceM (Maybe Expr)
rstepTraceAt whole path current =
  case collectArgsWithPaths path current of
    (Var f, _headPath, argsWithPaths) -> do
      defMap <- gets stDefs
      case Map.lookup f defMap of
        Just (Def _ matches) -> do
          record whole path

          result <- tryMatchesTrace whole matches argsWithPaths

          case result of
            Just (newCurrent, wholeAfterMatching) ->
              case replaceAt path newCurrent wholeAfterMatching of
                Just newWhole -> do
                  record newWhole path
                  return (Just newWhole)
                Nothing ->
                  return Nothing
            Nothing ->
              reduceSubexpr
        Nothing ->
          reduceSubexpr
    _ ->
      reduceSubexpr
  where
    reduceSubexpr =
      case current of
        e1 :$ e2 -> do
          left <- rstepTraceAt whole (path ++ [GoLeft]) e1
          case left of
            Just newWhole ->
              return (Just newWhole)
            Nothing ->
              rstepTraceAt whole (path ++ [GoRight]) e2
        _ ->
          return Nothing

-- Performs one traced reduction step on the whole expression.
matchPatTrace ::
  Expr ->
  Path ->
  Pat ->
  Expr ->
  ReduceM (Maybe ([(Name, Expr)], Expr), Expr)
matchPatTrace whole _ (PVar x) expr =
  return (Just ([(x, expr)], expr), whole)
matchPatTrace whole path (PApp c pats) expr =
  case collectArgsWithPaths path expr of
    (Con c', _headPath, argsWithPaths)
      | c == c' && length pats == length argsWithPaths -> do
          let triples =
                zipWith
                  (\pat (arg, argPath) -> (pat, arg, argPath))
                  pats
                  argsWithPaths

          (result, wholeAfter) <- matchPatsListTrace whole triples

          case result of
            Nothing ->
              return (Nothing, wholeAfter)
            Just (subs, _matchedArgs) -> do
              let exprAfter =
                    case subexprAt path wholeAfter of
                      Just e -> e
                      Nothing -> expr

              return (Just (subs, exprAfter), wholeAfter)
      | otherwise ->
          return (Nothing, whole)
    _ -> do
      ok <- consumeFuel
      if not ok
        then return (Nothing, whole)
        else do
          reducedWhole <- rstepTraceAt whole path expr
          case reducedWhole of
            Nothing ->
              return (Nothing, whole)
            Just whole' ->
              case subexprAt path whole' of
                Nothing ->
                  return (Nothing, whole')
                Just expr' ->
                  matchPatTrace whole' path (PApp c pats) expr'

-- Matches a list of patterns against arguments with their paths.
matchPatsListTrace ::
  Expr ->
  [(Pat, Expr, Path)] ->
  ReduceM (Maybe ([(Name, Expr)], [Expr]), Expr)
matchPatsListTrace whole [] =
  return (Just ([], []), whole)
matchPatsListTrace whole ((pat, expr, path) : rest) = do
  (first, whole1) <- matchPatTrace whole path pat expr

  case first of
    Nothing ->
      return (Nothing, whole1)
    Just (subs1, expr') -> do
      let refresh (p, oldExpr, pth) =
            case subexprAt pth whole1 of
              Just newExpr -> (p, newExpr, pth)
              Nothing -> (p, oldExpr, pth)

          rest' = map refresh rest

      (restResult, whole2) <- matchPatsListTrace whole1 rest'

      case restResult of
        Nothing ->
          return (Nothing, whole2)
        Just (subs2, exprs') ->
          return (Just (subs1 ++ subs2, expr' : exprs'), whole2)

-- Tries to apply one equation to the current arguments.
matchMatchTrace ::
  Expr ->
  Match ->
  [(Expr, Path)] ->
  ReduceM MatchAttempt
matchMatchTrace whole match argsWithPaths
  | length argsWithPaths < arity =
      return (MatchFailed whole)
  | otherwise = do
      counter <- gets stCounter
      let (renamedMatch, counter1) = renameMatch match counter
      putCounter counter1

      let pats = matchPats renamedMatch
          rhs = matchRhs renamedMatch
          (usedArgs, restArgs) = splitAt arity argsWithPaths
          triples =
            zipWith
              (\pat (arg, path) -> (pat, arg, path))
              pats
              usedArgs

      (result, wholeAfterMatching) <- matchPatsListTrace whole triples

      case result of
        Nothing ->
          return (MatchFailed wholeAfterMatching)
        Just (subs, _usedArgs') -> do
          let body = substMany subs rhs
          return (MatchSucceeded body (map fst restArgs) wholeAfterMatching)
  where
    arity = length (matchPats match)

-- Tries equations in order until one matches.
tryMatchesTrace ::
  Expr ->
  [Match] ->
  [(Expr, Path)] ->
  ReduceM (Maybe (Expr, Expr))
tryMatchesTrace _ [] _ =
  return Nothing
tryMatchesTrace whole (m : ms) argsWithPaths = do
  attempt <- matchMatchTrace whole m argsWithPaths

  case attempt of
    MatchSucceeded rhs restArgs wholeAfterMatching -> do
      let newCurrent = foldl' (:$) rhs restArgs
      return (Just (newCurrent, wholeAfterMatching))
    MatchFailed wholeAfterFailure -> do
      let refreshedArgs =
            [ (maybe arg id (subexprAt argPath wholeAfterFailure), argPath)
              | (arg, argPath) <- argsWithPaths
            ]

      tryMatchesTrace wholeAfterFailure ms refreshedArgs

-- Tries equations in order until one matches.
reduceTrace :: Expr -> ReduceM Expr
reduceTrace expr = do
  ok <- consumeFuel
  if not ok
    then return expr
    else do
      stepResult <- rstepTrace expr
      case stepResult of
        Nothing ->
          return expr
        Just expr' ->
          reduceTrace expr'

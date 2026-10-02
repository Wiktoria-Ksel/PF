module Context
  ( Dir (..),
    Path,
    Step (..),
    subexprAt,
    replaceAt,
    collectArgsWithPaths,
    collectArgs,
  )
where

import Syntax

-- Path to subexpression in the application tree.
data Dir = GoLeft | GoRight
  deriving (Eq, Show)

type Path = [Dir]

data Step = Step
  { stepExpr :: Expr,
    stepPath :: Path
  }
  deriving (Eq, Show)

-- Returns the subexpression at the given path.
subexprAt :: Path -> Expr -> Maybe Expr
subexprAt [] expr =
  Just expr
subexprAt (GoLeft : ds) (e1 :$ _) =
  subexprAt ds e1
subexprAt (GoRight : ds) (_ :$ e2) =
  subexprAt ds e2
subexprAt _ _ =
  Nothing

-- Replaces the subexpression at the given path.
replaceAt :: Path -> Expr -> Expr -> Maybe Expr
replaceAt [] newExpr _ =
  Just newExpr
replaceAt (GoLeft : ds) newExpr (e1 :$ e2) = do
  e1' <- replaceAt ds newExpr e1
  Just (e1' :$ e2)
replaceAt (GoRight : ds) newExpr (e1 :$ e2) = do
  e2' <- replaceAt ds newExpr e2
  Just (e1 :$ e2')
replaceAt _ _ _ =
  Nothing

-- Splits an application into its head and arguments
-- and keeps paths to them.
collectArgsWithPaths :: Path -> Expr -> (Expr, Path, [(Expr, Path)])
collectArgsWithPaths basePath expr =
  go basePath expr []
  where
    go path (e1 :$ e2) acc =
      go (path ++ [GoLeft]) e1 ((e2, path ++ [GoRight]) : acc)
    go path headExpr acc =
      (headExpr, path, acc)

fromAppsToList :: Expr -> [Expr] -> (Expr, [Expr])
fromAppsToList (e1 :$ e2) acc =
  fromAppsToList e1 (e2 : acc)
fromAppsToList f acc =
  (f, acc)

-- Splits an application into its head and arguments.
collectArgs :: Expr -> (Expr, [Expr])
collectArgs e =
  fromAppsToList e []

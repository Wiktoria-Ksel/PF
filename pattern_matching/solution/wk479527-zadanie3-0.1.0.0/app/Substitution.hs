module Substitution
  ( subst,
    substMany,
    renameMatch,
    renamePat,
    patVars,
  )
where

import Data.List (foldl')
import Syntax


-- Substitutes a variable with an expression.
subst :: (Name, Expr) -> Expr -> Expr
subst (x, val) (Var y)
  | x == y = val
  | otherwise = Var y
subst _ (Con c) = Con c
subst s (e1 :$ e2) = subst s e1 :$ subst s e2

-- Applies a list of substitutions.
substMany :: [(Name, Expr)] -> Expr -> Expr
substMany subs expr = foldl' (flip subst) expr subs


-- Renames variables in an equation to fresh names.
renameMatch :: Match -> Int -> (Match, Int)
renameMatch (Match name pats rhs) i =
  let vars = concatMap patVars pats
      newVars = zipWith (\v n -> v ++ "_" ++ show (i + n)) vars [1 ..]
      subs = zip vars (map Var newVars)
      newPats = map (renamePat subs) pats
      newRhs = substMany subs rhs
   in (Match name newPats newRhs, i + length vars)

-- Renames variables in a pattern.
renamePat :: [(Name, Expr)] -> Pat -> Pat
renamePat subs (PVar x) =
  case lookup x subs of
    Just (Var newName) -> PVar newName
    _ -> PVar x
renamePat subs (PApp name pats) =
  PApp name (map (renamePat subs) pats)

-- Collects variable names from a pattern.
patVars :: Pat -> [Name]
patVars (PVar x) = [x]
patVars (PApp _ ps) = concatMap patVars ps

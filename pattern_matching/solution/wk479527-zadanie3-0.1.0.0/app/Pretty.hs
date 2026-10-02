module Pretty
  ( showWithFocus,
  )
where

import Context
import Syntax

-- Shows an expression with braces around the focused subexpression.
showWithFocus :: Path -> Expr -> String
showWithFocus path expr =
  showsWithFocus path 0 expr ""

-- Helper that carries precedence while printing.
showsWithFocus :: Path -> Int -> Expr -> ShowS
showsWithFocus [] _ expr =
  showChar '{' . showsPrec 0 expr . showChar '}'
showsWithFocus _ _ (Var x) =
  showString x
showsWithFocus _ _ (Con c) =
  showString c
showsWithFocus (GoLeft : ds) p (e1 :$ e2) =
  showParen (p > 10) $
    showsWithFocus ds 10 e1
      . showChar ' '
      . showsPrec 11 e2
showsWithFocus (GoRight : ds) p (e1 :$ e2) =
  showParen (p > 10) $
    showsPrec 10 e1
      . showChar ' '
      . showsWithFocus ds 11 e2

module Syntax where

import qualified Data.Map as Map

data Def = Def
  { defName :: Name,
    defMatches :: [Match]
  }
  deriving (Eq, Show)

data Match = Match
  { matchName :: Name,
    matchPats :: [Pat],
    matchRhs :: Expr
  }
  deriving (Eq, Show)

infixl 9 :$

data Expr
  = Var Name
  | Con Name
  | Expr :$ Expr
  deriving (Eq)

data Pat
  = PVar Name
  | PApp Name [Pat]
  deriving (Eq, Show)

newtype Prog = Prog [Def]
  deriving (Eq, Show)

type Name = String

type DefMap = Map.Map Name Def

instance Show Expr where
  showsPrec _ (Var x) = showString x
  showsPrec _ (Con x) = showString x
  showsPrec p (e1 :$ e2) =
    showParen (p > 10) $
      showsPrec 10 e1 . showChar ' ' . showsPrec 11 e2

buildDefMap :: Prog -> DefMap
buildDefMap (Prog defs) =
  Map.fromList [(defName d, d) | d <- defs]

{-# LANGUAGE PackageImports #-}

module Parser
  ( fromHsString,
  )
where

import Data.List (foldl')
import qualified Data.Map as Map
import Syntax
import "haskell-src" Language.Haskell.Parser
import "haskell-src" Language.Haskell.Syntax

fromHsString :: String -> Prog
fromHsString = fromParseResult . parseModule

fromParseResult :: ParseResult HsModule -> Prog
fromParseResult (ParseOk a) = fromHsModule a
fromParseResult (ParseFailed _ err) = error err

fromHsModule :: HsModule -> Prog
fromHsModule (HsModule _ _ _ _ decls) =
  Prog (groupMatches (concatMap fromDecl decls))

-- Converts a declaration into one or more equations.
fromDecl :: HsDecl -> [Match]
fromDecl (HsFunBind matches) =
  map fromMatch matches
fromDecl (HsPatBind _ (HsPVar name) (HsUnGuardedRhs body) _) =
  [ Match
      { matchName = nameToString name,
        matchPats = [],
        matchRhs = fromHsExpr body
      }
  ]
fromDecl _ = error "Unsupported declaration."

fromMatch :: HsMatch -> Match
fromMatch (HsMatch _ name pats (HsUnGuardedRhs body) _) =
  Match
    { matchName = nameToString name,
      matchPats = map fromHsPat pats,
      matchRhs = fromHsExpr body
    }
fromMatch _ =
  error "Unsupported match."

-- Groups equations by name, 
-- preserving equation order within each definition.
groupMatches :: [Match] -> [Def]
groupMatches ms = map makeDef (Map.toList groupedPairs)
  where
    groupedPairs = foldl' addMatch Map.empty ms
    addMatch acc m =
      Map.insertWith (\new old -> old ++ new) (matchName m) [m] acc
    makeDef (name, matches) = Def name matches

nameToString :: HsName -> String
nameToString (HsIdent x) = x
nameToString (HsSymbol x) = x

fromHsPat :: HsPat -> Pat
fromHsPat (HsPVar name) =
  PVar (nameToString name)
fromHsPat (HsPApp (UnQual name) pats) =
  PApp (nameToString name) (map fromHsPat pats)
fromHsPat (HsPParen p) =
  fromHsPat p
fromHsPat p =
  error $ buildStr (showString "Unsupported pattern: " . shows p)

fromHsExpr :: HsExp -> Expr
fromHsExpr (HsVar (UnQual name)) = Var (nameToString name)
fromHsExpr (HsCon (UnQual name)) = Con (nameToString name)
fromHsExpr (HsApp e1 e2) = fromHsExpr e1 :$ fromHsExpr e2
fromHsExpr (HsParen e) = fromHsExpr e
fromHsExpr e = error $ buildStr (showString "Unsupported expression type: " . shows e)

buildStr :: ShowS -> String
buildStr s = s ""

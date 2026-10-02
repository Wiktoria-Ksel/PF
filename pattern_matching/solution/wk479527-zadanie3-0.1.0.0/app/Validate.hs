module Validate
  ( validateProg,
  )
where

import Substitution (patVars)
import Syntax

hasDuplicates :: (Eq a) => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x : xs) = x `elem` xs || hasDuplicates xs

validateProg :: Prog -> Maybe String
validateProg (Prog defs) =
  case filter isMainDef defs of
    [Def _ [Match _ [] _]] ->
      checkAllArities defs
    [] ->
      Just "Error: Missing main definition."
    _ ->
      Just "Error: main must have exactly one equation and no arguments."
  where
    isMainDef (Def name _) = name == "main"

    checkAllArities [] = Nothing
    checkAllArities (Def name matches : rest) =
      case checkDefArities name matches of
        Just err -> Just err
        Nothing -> checkAllArities rest

-- Checks that all equations of one definition have the same arity.
checkDefArities :: Name -> [Match] -> Maybe String
checkDefArities name [] =
  Just $ "Error: empty definition of " ++ name
checkDefArities name (m : ms) =
  let arity = length (matchPats m)
   in if all (\m' -> length (matchPats m') == arity) ms
        then checkPatternVars name (m : ms)
        else Just $ "Error: equations of " ++ name ++ " have different numbers of arguments."

-- Checks that pattern variables are not repeated in one equation.
checkPatternVars :: Name -> [Match] -> Maybe String
checkPatternVars _ [] =
  Nothing
checkPatternVars name (m : ms) =
  let vars = concatMap patVars (matchPats m)
   in if hasDuplicates vars
        then Just $ "Error: duplicate pattern variables in definition of " ++ name
        else checkPatternVars name ms

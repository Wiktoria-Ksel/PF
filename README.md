# Functional Programming

A Haskell-like language interpreter written in Haskell, developed through three stages of a university functional programming assignment.

## Haskell-like Language Interpreter

The interpreter parses a small, untyped language using a subset of Haskell syntax and displays the reduction of its `main` expression.

- **Parsing:** `haskell-src` parses the input, which is converted into a custom abstract syntax tree.
- **Evaluation:** normal-order reduction, function application and ordered equations with variable and constructor patterns. Arguments are reduced when needed for pattern matching.
- **Substitution:** variable renaming and substitution when applying function definitions.
- **Tracing:** a `State` monad tracks definitions, fresh-variable identifiers, evaluation fuel and reduction history. Expression contexts identify and highlight the current redex.
- **Validation:** checks include a single zero-argument `main` equation, consistent equation arity and duplicate variables in patterns.

**Tools:** Haskell 2010, GHC, Cabal, `haskell-src`, `containers`, `mtl`.

## Source code

| Component | Location |
| --- | --- |
| Command-line entry point | [Main.hs](pattern_matching/solution/wk479527-zadanie3-0.1.0.0/app/Main.hs) |
| Parser and syntax | [Parser.hs](pattern_matching/solution/wk479527-zadanie3-0.1.0.0/app/Parser.hs) · [Syntax.hs](pattern_matching/solution/wk479527-zadanie3-0.1.0.0/app/Syntax.hs) |
| Reduction and pattern matching | [ReducerTrace.hs](pattern_matching/solution/wk479527-zadanie3-0.1.0.0/app/ReducerTrace.hs) |
| Substitution and variable renaming | [Substitution.hs](pattern_matching/solution/wk479527-zadanie3-0.1.0.0/app/Substitution.hs) |
| Reduction history and output | [Trace.hs](pattern_matching/solution/wk479527-zadanie3-0.1.0.0/app/Trace.hs) · [Context.hs](pattern_matching/solution/wk479527-zadanie3-0.1.0.0/app/Context.hs) · [Pretty.hs](pattern_matching/solution/wk479527-zadanie3-0.1.0.0/app/Pretty.hs) |
| Build configuration | [Cabal package](pattern_matching/solution/wk479527-zadanie3-0.1.0.0/wk479527-zadanie3.cabal) |

The source files are available directly in the repository. This implementation is the final assignment stage.

## Build and run

Requirements: GHC with `base >= 4.15.1.0` and Cabal 3.8 or newer. Cabal resolves the dependencies listed in the package file. Run from the repository root:

```sh
cd pattern_matching/solution/wk479527-zadanie3-0.1.0.0
cabal build
cabal run zadanie3 -- --help
cabal run zadanie3 -- ../../examples/peano.hs
```

The [example input](pattern_matching/examples/peano.hs) adds two Peano numbers:

```haskell
one = S Z
two = S one
add Z n = n
add (S m) n = S (add m n)
main = add two two
```

This is input for the interpreter: constructor names such as `S` and `Z` do not need Haskell data declarations. The program prints the input followed by a reduction trace with the selected redex highlighted.

Input can also be read from standard input:

```sh
cabal run zadanie3 < ../../examples/peano.hs
```

The current entry point uses a fixed fuel budget of 30 and displays at most 30 trace entries. The supported syntax and validation rules are described in the assignment statements.

## Assignment statements

- [Stage 1](pattern_matching/statements/PF-zadanie-1.md)
- [Stage 2](pattern_matching/statements/PF-zadanie-2.md)
- [Stage 3](pattern_matching/statements/PF-zadanie-3.md)

The original statements are in Polish.

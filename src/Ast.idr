{-
  An embeded DSL for prolog-style logic programming, based on the
  pure-prolog parser and interpreter originally written in Haskell

  Original author: Ken Friis Larsen      <kflarsen@diku.dk>
  Translated into Idris by Nathan Bedell <nbedell@tulane.edu>
-}

module Ast

%access public export

----------------------------------------------------------------------
-- Abstract Syntax Tree
----------------------------------------------------------------------

Atom : Type
Atom = String

Variable : Type
Variable = String

data Term : Type where
 Var : Variable -> Term
 Comp : Atom -> List Term -> Term

Terms : Type
Terms = List Term

Clause : Type
Clause = (Term, Terms) -- head and body

Clauses : Type
Clauses = List Clause

Goal : Type
Goal = List Term

Program : Type
Program = Clauses
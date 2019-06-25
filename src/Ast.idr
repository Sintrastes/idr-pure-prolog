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

and' : List Bool -> Bool
and' [] = True
and' xs = foldr1 p xs
  where p True True = True
        p x y = False

implementation Eq Term where
  (Var x) == (Var y) = assert_total $ x == y
  (Comp x y) == (Comp x' y') = assert_total $ (x == x') && (and' $ Prelude.List.zipWith (==) y y')
  x == y = assert_total $ False

implementation Show Term where
  show (Var s) = s
  show (Comp s []) = s
  show (Comp s xs) = s ++ "(" ++ (concat $ intersperse "," (map show xs)) ++ ")"

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
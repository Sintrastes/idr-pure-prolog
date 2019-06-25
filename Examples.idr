{-
  An embeded DSL for prolog-style logic programming, based on the
  pure-prolog parser and interpreter originally written in Haskell

  Original author: Ken Friis Larsen      <kflarsen@diku.dk>
  Translated into Idris by Nathan Bedell <nbedell@tulane.edu>
-}

p : Term
p = Comp "p" []

q : Term
q = Comp "q" []

x : Term
x = Comp "x" []

z : Term
z = Comp "z" []

r : Term -> Term
r x = Comp "r" [x]

n : Term -> Term
n x = Comp "n" [x]

X : Term
X = Var "X"

s : Term -> Term
s x = Comp "s" [x]

f : Term -> Term -> Term
f x y = Comp "f" [x, y]

program : Program
program = [ p .
          , q :- [p]
	  , (n z) .
	  , (n (s X)) :- [n X]
	  , (f z z) .
	  , (f z (s z)) .
	  , (f X X) .
	  ]

result : List Solution
result = nub $ bfs $ makeReportTree program [f z X]

-- Note: I should make this more type-safe.
getSolutions : List Solution -> List Term
getSolutions xs = concat $ map getSolution xs
  where getSolution (MkSolution x) = map snd x

f_val : Term -> List Term
f_val x = getSolutions $ nub $ bfs $ makeReportTree program [f x X]

nat : Term -> Bool
nat t = query /= []
  where query : List Solution
        query = ?- n t

-- This is a version of f_val that runs on our embedded prolog
-- system, but only allows values of type Nat.
f_val' : (x : Term) -> {auto p : True = nat x} -> List Term
f_val' x = getSolutions $ ?- f x X

-- An example. The following (should) compile.
-- However, the compiler doesn't like when I try to assert this total. It does strange things.
-- val : List Term
-- val = f_val' z

-- The following does not compile.
-- val' : List Term
-- val' = f_val' (s z)
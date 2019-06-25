{-
  An embeded DSL for prolog-style logic programming, based on the
  pure-prolog parser and interpreter originally written in Haskell

  Original author: Ken Friis Larsen      <kflarsen@diku.dk>
  Translated into Idris by Nathan Bedell <nbedell@tulane.edu>
-}

module Interp

import Ast

%access public export

----------------------------------------------------------------------
-- Interpreter
----------------------------------------------------------------------

Unifier : Type
Unifier = List (Variable, Term)

-- A non-lazy version of maybe
maybe' : b -> (a -> b) -> Maybe a -> b
maybe' n j Nothing  = n
maybe' n j (Just x) = j x

subs : Unifier -> Term -> Term
subs u (Var x)   = maybe' (Var x) id (lookup x u)
subs u (Comp n ts) = Comp n (map (subs u) ts)

compose : Unifier -> Unifier -> Unifier
compose u1 u2 = (map (\(v, t) => (v, subs u2 t)) u1) ++ u2

occursIn : Variable -> Term -> Bool
occursIn v (Var x)     = v == x
occursIn v (Comp _ ms) = any (occursIn v) ms

mutual
  unify : Term -> Term -> Maybe Unifier
  unify (Var "_") t                         = pure []
  unify t (Var "_")                         = pure []
  unify (Var x) (Var y) with (x == y)
     unify (Var x) (Var y) | True           = pure [] 
  unify (Var x) t with (not (x `occursIn` t))
      unify (Var x) t   | True              = pure [(x, t)]
  unify t v@(Var _)                         = unify v t
  unify (Comp m ms) (Comp n ns) with (m == n)
     unify (Comp m ms) (Comp n ns) | True   = unifyList ms ns
  unify _ _ = Nothing

  unifyList : Terms -> Terms -> Maybe Unifier
  unifyList (t :: ts) (r :: rs) =
      do u1 <- unify t r
         u2 <- unifyList (map (subs u1) ts) (map (subs u1) rs)
  -- Note: I'm not sure if Idris supports backtick notation for operators
         pure $ u1 `compose` u2
  unifyList [] [] = Just []
  unifyList _ _   = Nothing

mutual
  vars : Term -> List Variable
  vars (Var "_") = []
  vars (Var x) = [x]
  vars (Comp _ ts) = varsList ts

  varsList : Terms -> List Variable
  varsList ts = [ v | t <- ts, v <- vars t]

variables : Terms -> List Variable
variables ts = nub $ varsList ts

freshen : List Variable -> (Term, Terms) -> (Term, Terms)
freshen bound (tc, tb) = (subs sub tc, map (subs sub) tb)
    where vars : List Variable
          vars = variables (tc :: tb)
          sub = [ (v, Var $ nextVar 0 v) | v <- vars, v `elem` bound]
	  nextVar : Int -> Variable -> Variable
          nextVar i v = let v' = "_" ++ show i ++ "_" ++ v in
                        if v' `elem` bound then nextVar (i+1) v
                        else v'


data Solution : Type where
    MkSolution : List (Variable, Term) -> Solution

implementation Eq Solution where
    (MkSolution x) == (MkSolution y) = assert_total $ x == y
    x == y = assert_total $ False


Show Solution where
  show (MkSolution [])            = "True"
  show (MkSolution xs ) = (concat $ intersperse "\n" $ map renderBindings xs)
    where renderBindings (v, t) = v ++ " = " ++ show t


isPlain : List Char -> Bool
isPlain (c :: cs) = isLower c && all (\c => c == '_' || isAlphaNum c) cs
isPlain [] = False
isPlain _ = False

data SearchTree : Type where
  Sol : Solution -> SearchTree
  Node : Goal -> List SearchTree -> SearchTree

Eq SearchTree where
  (Sol x) == (Sol y) = assert_total $ x == y
  (Node x y) == (Node x' y') = assert_total $ (x == x') && (y == y')
  x == y = False

isReportGoal : Term -> Bool
isReportGoal (Comp "_report" _) = True
isReportGoal _                  = False

getSolution : Term -> Solution
getSolution (Comp "_report" args) = MkSolution sol
    where nontriv : (String, Term) -> Bool
          nontriv (x, (Var y)) with (x == y)
	      nontriv (x, (Var y)) | True = False
          nontriv _ = True
	  sol = filter nontriv $ map (\ (Comp "=" [Comp v [], t]) => (v, t)) args
getSolution _ = MkSolution [("ERROR", Var "_Error")]

-- Uses the List monad for backtracking
solve : Program -> Goal -> List SearchTree
solve prog [r] with (isReportGoal r)
    solve prog [r] | True  =  pure $ Sol $ getSolution r
    solve prog [r] | False = [] 
solve prog g@(t1 :: ts) = pure $ Node g trees
    where trees = do c <- prog
                     let (tc, tsc) = freshen (variables g) c
                     case unify tc t1 of
                       Just u => do
                         let g' = map (subs u) $ tsc ++ ts
                         solve prog g'
                       Nothing => []

makeReportGoal : Terms -> List Term
makeReportGoal goal = [Comp "_report" reportVars]
    where vars : List Variable
          vars = variables goal
	  reportVars : Terms
          reportVars = map (\ v => Comp "=" [Comp v [], Var v]) vars

-- Use the trick of inserting an extra reporting goal
makeReportTree : Program -> Goal -> SearchTree
makeReportTree prog goal = Node goal $ solve prog (goal ++ makeReportGoal goal)


----------------------------------------------------------------------
-- Traveral of Search Trees
----------------------------------------------------------------------

-- Depth first
dfs : SearchTree -> List Solution
dfs (Sol sols) = [sols]
dfs (Node _ st) = [ s | t <- st, s <- dfs t]

-- Breath first
bfs : SearchTree -> List Solution
bfs t = trav [t]
    where trav [] = []
          trav ((Sol x) :: q) = x :: trav q
          trav ((Node _ st)  :: q) = trav (q ++ st)

-- Limited depth first
limitedDfs : Int -> SearchTree -> List Solution
limitedDfs _ (Sol sols)  = [sols]
limitedDfs 0 _           = []
limitedDfs n (Node _ st) = [ s | t <- st, s <- limitedDfs (n-1) t]

-----------------------------------------------------------------------
--- Syntax for the DSL ---
-----------------------------------------------------------------------

-- Note: I might actually want to do something here like apply some
-- function to "h" and "b" here in order to supply some more syntatic
-- sugar.

syntax [h] ":-" [b] = (h, b)

empty : List Term
empty = []

syntax [p] "." = (p, empty)

syntax "?-" [q] = nub $ bfs $ makeReportTree program [q]


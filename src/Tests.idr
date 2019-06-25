module Main

import Interp
import Ast

program2 : Program
program2 = [ (r z) .
           , (r (r z)) .
	   ]

-- Some tests
main : IO ()
main = do
  putStrLn "\n--------------"
  print $ (nub $ dfs $ makeReportTree [(n z) .] [n X])
  putStrLn "\n---------------"
  print $ (nub $ bfs $ makeReportTree [(n z) .] [n X])
  putStrLn "\n---------------"
  -- Why does this one get [True, True]? It matches twice?
  print $ (nub $ bfs $ makeReportTree program2 [r X])
  putStrLn "\n---------------"
  print result
  putStrLn "\n---------------"
  print $ f_val z
  putStrLn "\n---------------"
  print $ f_val (s z)
  putStrLn "\n---------------"
  print $ f_val (s (s z))
  putStrLn "\n---------------"
  print $ nat z
  putStrLn "\n---------------"
  print $ nat (s z)
  putStrLn "\n---------------"
  print $ nat p
  putStrLn "\n---------------"
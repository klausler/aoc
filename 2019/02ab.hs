import IntCode
import qualified Data.Map as M
main = do
  input <- readFile "in/02.txt"
  let codes = (read $ '[' : input ++ "]") :: [Int]
      m = load codes
      runnv n v = run (M.insert 1 n $ M.insert 2 v m) 0
  print $ runnv 12 2 -- part A
  print $ head [ 100 * n + v | n <- [0..99], v <- [0..99], runnv n v == 19690720 ] -- part B

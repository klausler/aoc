import IntCode
import qualified Data.Map as M
main = do
  m <- load <$> readFile "in/02.txt"
  let runnv n v = (fst $ run (M.insert 1 n $ M.insert 2 v m) 0 0 []) M.! 0
  print $ runnv 12 2 -- part A
  print $ head [ 100 * n + v | n <- [0..99], v <- [0..99], runnv n v == 19690720 ] -- part B

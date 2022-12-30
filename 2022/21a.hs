import qualified Data.Map as M
val m [j] = read j
val m [x,"+",y] = m x + m y
val m [x,"-",y] = m x - m y
val m [x,"*",y] = m x * m y
val m [x,"/",y] = m x `div` m y
main = do
  input <- lines <$> readFile "in/21.txt"
  let m = M.fromList [ (init name, val (m M.!) rest) | (name:rest) <- words <$> input ]
  print $ m M.! "root"

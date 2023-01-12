import qualified Data.Map as M
dist m "COM" = 0
dist m x = let y = m M.! x in 1 + dist m y
common m x y dx dy
  | x == y = x
  | dx > dy = common m (m M.! x) y (dx - 1) dy
  | dx < dy = common m x (m M.! y) dy (dy - 1)
  | otherwise = common m (m M.! x) (m M.! y) (dx - 1) (dy - 1)
main = do
  input <- lines <$> readFile "in/06.txt"
  let m = M.fromList [ (b,a)  | ln <- input, let (a,(')':b)) = span (/=')') ln ]
      youO = m M.! "YOU"
      sanO = m M.! "SAN"
      com = common m youO sanO (dist m youO) (dist m sanO)
  print $ sum $ dist m <$> M.keys m -- part A
  print $ dist m youO + dist m sanO - 2 * dist m com -- part B


import Data.List(sort)
combine :: [[Int]] -> [[Int]]
combine [] = []
combine ([]:rest) = combine rest
combine (x:[]:rest) = x : combine rest
combine (x:rest)
  | (r:rest') <- combine rest = (x ++ r) : rest'
  | otherwise = [x]
main = do
  input <- readFile "in/01.txt"
  let combined = combine $ ((read <$>) . words) <$> lines input
      descending = reverse $ sort $ sum <$> combined
  print $ head descending -- part A
  print $ sum $ take 3 descending -- part B

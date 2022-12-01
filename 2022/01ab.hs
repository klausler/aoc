import Data.List(groupBy, sort)
combine2 :: [[Int]] -> [[Int]]
combine2 vals = filter (not . null) $ concat <$> groupBy (\x y -> null x == null y) vals
main = do
  input <- readFile "in/01.txt"
  let combined = combine2 $ ((read <$>) . words) <$> lines input
      descending = reverse $ sort $ sum <$> combined
  print $ head descending -- part A
  print $ sum $ take 3 descending -- part B

import Data.List(groupBy, sort)
gather = filter (not.null) . (concat <$>) . groupBy (\x y -> null x == null y)
tally = sum . (read <$>) :: [String] -> Int
main = do
  input <- (words <$>) <$> lines <$> readFile "in/01.txt"
  let descending = reverse $ sort $ tally <$> gather input
  print $ head descending -- part A
  print $ sum $ take 3 descending -- part B

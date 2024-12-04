isSafe (report@(x:(rest@(y:_))))
  | x >= y = all ok $ zipWith (-) report rest
  | otherwise = all ok $ zipWith (-) rest report
  where ok diff = diff >= 1 && diff <= 3
omissions [] = [[]]
omissions (x:xs) = xs : ((x:) <$> omissions xs)
main = do
  lns <- lines <$> readFile "in/02.txt"
  let reports = (fmap read . words) <$> lns
  print $ length $ filter isSafe reports -- part 1
  print $ length $ filter (any isSafe . omissions) reports -- part 2

main = do
  lns <- lines <$> readFile "in/08.txt"
  let outs = (drop 11 . words) <$> lns
      easy wd = length wd `elem` [2, 3, 4, 7, 8]
  print $ length $ filter easy $ concat outs

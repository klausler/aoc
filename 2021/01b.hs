windows n m [] = [n+m]
windows n m (h:t) = n+m+h : windows m h t
main = do
  ds <- (read <$>) <$> lines <$> readFile "in/01.txt"
  let ws = drop 2 $ windows 0 0 ds
  print $ length $ filter id $ zipWith (>) (tail ws) ws

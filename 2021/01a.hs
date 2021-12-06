readInt = read :: String -> Int
main = do
  ds <- (readInt <$>) <$>  lines <$> readFile "in/01.txt"
  print $ length $ filter id $ zipWith (>) (tail ds) ds

import Data.List(transpose)
toInt bits = sum $ zipWith (*) (iterate (2*) 1) $ reverse bits
main = do
  lns <- lines <$> readFile "in/03.txt"
  let nVals = length lns
      ones = (length . filter (=='1')) <$> transpose lns
      most = [ if n*2 >= nVals then 1 else 0 | n <- ones ]
      gamma = toInt most
      epsilon = toInt $ (1-) <$> most
  print $ gamma * epsilon

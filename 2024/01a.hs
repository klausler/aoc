import Data.List(sort)
main = do
  lns <- lines <$> readFile "in/01.txt"
  let (xs,ys) = unzip [ (read x, read y) | [x,y] <- words <$> lns ]
      (sxs,sys) = (sort xs, sort ys) :: ([Int],[Int])
  print $ sum $ abs <$> zipWith (-) sxs sys

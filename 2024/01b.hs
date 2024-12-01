import qualified Data.Map as M
main = do
  lns <- lines <$> readFile "in/01.txt"
  let (xs,ys) = unzip [ (read x, read y) | [x,y] <- words <$> lns ]
      yM = M.fromListWith (+) [ (y,1) | y <- ys ]
  print $ sum [ x * M.findWithDefault 0 x yM | x <- xs ]

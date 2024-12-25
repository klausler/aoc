import Data.List(transpose)
groups [] = []
groups lns = let (a,b) = splitAt 8 lns in (tail $ a) : groups b
toNums ch g = map (length . filter (ch==)) $ transpose g
main = do
  lns <- lines <$> readFile "in/25.txt"
  let grps = groups $ ("" : lns)
      locks = map (toNums '.') $ filter (("#####"==).head) grps
      keys  = map (toNums '#') $ filter (("....."==).head) grps
  print $ sum [ 1 | l <- locks, k <- keys, and $ zipWith (<=) k l ]

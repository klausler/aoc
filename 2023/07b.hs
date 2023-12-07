import Data.List(elemIndex,findIndex,group,sort,sortBy)
((.*.) `on` f) x y = f x .*. f y
fromJust (Just x) = x
cVal c = fromJust $ elemIndex c "J23456789TQKA"
score h = fromJust $ findIndex (cts `elem`)
     [[[1,1,1,1,1]], [[1,1,1,2],[1,1,1,1]], [[1,2,2]],
      [[1,1,3],[1,1,2],[1,1,1]], [[2,3],[2,2]],
      [[1,4],[1,3],[1,2],[1,1]], [[5],[4],[3],[2],[1],[]]]
  where cts = sort $ map length $ group $ sort $ filter (/='J') h
cmp a b = if as == bs then ccmp else compare as bs
  where (as,bs) = (score a, score b)
        ccmp = head $ dropWhile (==EQ) $ zipWith (compare `on` cVal) a b
main = do
  lns <- lines <$> readFile "in/07.txt"
  let handBids = [ (h, read b) | ln <- lns, let [h,b] = words ln ]
      sorted = sortBy (cmp `on` fst) handBids
  print $ sum $ zipWith (*) [1..] $ map snd sorted -- part B

import Data.List(elemIndex,findIndex,group,sort,sortBy)
((.*.) `on` f) x y = f x .*. f y
fromJust (Just x) = x
cValA c = fromJust $ elemIndex c "23456789TJQKA"
cValB c = fromJust $ elemIndex c "J23456789TQKA"
scoreA h = fromJust $ elemIndex cts [[1,1,1,1,1],[1,1,1,2],[1,2,2],[1,1,3],[2,3],[1,4],[5]]
  where cts = sort $ map length $ group $ sort h
scoreB h = fromJust $ findIndex (cts `elem`)
     [[[1,1,1,1,1]], [[1,1,1,2],[1,1,1,1]], [[1,2,2]],
      [[1,1,3],[1,1,2],[1,1,1]], [[2,3],[2,2]],
      [[1,4],[1,3],[1,2],[1,1]], [[5],[4],[3],[2],[1],[]]]
  where cts = sort $ map length $ group $ sort $ filter (/='J') h
cmp score cVal a b = if as == bs then ccmp else compare as bs
  where (as,bs) = (score a, score b)
        ccmp = head $ dropWhile (==EQ) $ zipWith (compare `on` cVal) a b
main = do
  lns <- lines <$> readFile "in/07.txt"
  let handBids = [ (h, read b) | ln <- lns, let [h,b] = words ln ]
      sortedA = sortBy (cmp scoreA cValA `on` fst) handBids
      sortedB = sortBy (cmp scoreB cValB `on` fst) handBids
  print $ sum $ zipWith (*) [1..] $ map snd sortedA -- part A
  print $ sum $ zipWith (*) [1..] $ map snd sortedB -- part B

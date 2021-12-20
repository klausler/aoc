import Data.Array
chMap '.' = 0
chMap '#' = 1
reframe border img = array ((1,1),(rows+2,cols+2)) $
    [ ((r,c),border) | r <- [1, rows+2], c <- [1..cols+2] ] ++
    [ ((r,c),border) | r <- [1..rows+2], c <- [1, cols+2] ] ++
    [ ((r+1,c+1),x) | ((r,c),x) <- assocs img ]
  where ((1,1),(rows,cols)) = bounds img
twoPows = iterate (2*) 1
pixelToIndex img r c = sum $ zipWith (*) twoPows [ img ! (r+r',c+c') | r' <- [1,0,-1], c' <- [1,0,-1] ]
process algo img = array bds [ ((r,c), new r c) | (r,c) <- indices img ]
  where
    bds@((1,1),(rows,cols)) = bounds img
    border = algo $ if img ! (1,1) == 0 then 0 else 511
    new r c
      | r > 1, r < rows, c > 1, c < cols = algo $ pixelToIndex img r c
      | otherwise = border
main = do
  (line1:_:lns) <- lines <$> readFile "in/20.txt"
  let algo = ((listArray (0, 511) $ chMap <$> line1) !)
      img0 = listArray ((1,1),(length lns, length $ head lns)) $ chMap <$> concat lns
      framed = (iterate (reframe 0) img0) !! 51
      story = iterate (process algo) framed
  print $ sum $ elems $ story !! 2 -- part 1
  print $ sum $ elems $ story !! 50 -- part 2

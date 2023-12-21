import qualified Data.Set as S
neighbors plots rows columns (r,c) = [ (r',c') | (r',c') <- [(r-1,c),(r+1,c),(r,c-1),(r,c+1)], (r' `mod` rows, c' `mod` columns) `S.member` plots ]
step plots rows columns set = S.fromList $ concat [ neighbors plots rows columns point | point <- S.toList set ]
lagrange xs ys x =
  sum [ yj * product [ (x - xi) / (xj - xi) | (i,xi) <- zip [0..] xs, i/=j ]
      | (j,xj,yj) <- zip3 [0..] xs ys ]
main = do
  lns <- lines <$> readFile "in/21.txt"
  let plots = S.fromList $ [ (r,c) | (r,ln) <- zip [0..] lns, (c,ch) <- zip [0..] ln, ch /= '#' ]
      (rows,columns) = (length lns, maximum $ length <$> lns)
      [(start@(startR,startC))] = [ (r,c) | (r,ln) <- zip [0..] lns, (c,ch) <- zip [0..] ln, ch == 'S' ]
      sizes = map S.size $ iterate (step plots rows columns) $ S.singleton start
  print $ sizes !! 64 -- part A
  let True = 2 * startR + 1 == rows -- confirm that start is in center
      xs = take 3 $ drop 2 [ rows-(startR+1), 2*rows-(startR+1).. ]
      ys = (map fromIntegral $ map (sizes !!) xs) :: [Rational]
  print $ lagrange (map fromIntegral xs) ys 26501365 -- part B

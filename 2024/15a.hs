import qualified Data.Set as S
survey f lns = [ (r,c) | (r,ln) <- zip [0..] lns, (c,ch) <- zip [0..] ln, f ch ]
toDir '^' = (-1,0)
toDir '>' = (0,1)
toDir 'v' = (1,0)
toDir '<' = (0,-1)
canMove space boxes (rc@(r,c)) (drc@(dr,dc)) n
  | rc' `S.member` boxes = canMove space boxes rc drc (n+1)
  | rc' `S.member` space = n
  | otherwise = 0
  where rc' = (r+n*dr,c+n*dc)
run space boxes robot [] = boxes
run space boxes (rc@(r,c)) (mv:mvs)
  | d == 0 = run space boxes rc mvs
  | d == 1 = run space boxes rc' mvs
  | otherwise = run space boxes' rc' mvs
  where
    (drc@(dr,dc)) = toDir mv
    rc' = (r+dr,c+dc)
    d = canMove space boxes rc drc 1
    boxes' = S.insert (r+d*dr,c+d*dc) $ S.delete rc' boxes
gps (r,c) = 100 * r + c
main = do
  lns <- lines <$> readFile "in/15.txt"
  let (m,(_:moves')) = break null lns
      moves = concat moves'
      space = S.fromList $ survey (/='#') m
      boxes = S.fromList $ survey (=='O') m
      [robot] = survey (=='@') m
      boxes' = run space boxes robot moves
  print $ sum $ map gps $ S.toList boxes' -- part 1

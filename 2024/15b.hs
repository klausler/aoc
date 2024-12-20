import qualified Data.Set as S
survey f lns = [ (r,c) | (r,ln) <- zip [0..] lns, (c,ch) <- zip [0,2..] ln, f ch ]
toDir '^' = (-1,0)
toDir '>' = (0,1)
toDir 'v' = (1,0)
toDir '<' = (0,-1)
isLeftBox boxes (r,c)
  | (r,c-1) `S.member` boxes = not $ isLeftBox boxes (r,c-1)
  | otherwise = True
canMove space boxes (rc@(r,c)) (drc@(dr,dc))
  | rc' `S.notMember` space = [ ]
  | rc' `S.notMember` boxes = [ S.empty ]
  | dr == 0 = [ S.insert rc' bs | bs <- canMove space boxes rc' drc ]
  | isLeftBox boxes rc' = [ S.insert rc' $ S.insert right $ S.union bs1 bs2
                          | bs1 <- canMove space boxes rc' drc, bs2 <- canMove space boxes right drc ]
  | otherwise = [ S.insert left $ S.insert rc' $ S.union bs1 bs2
                | bs1 <- canMove space boxes left drc, bs2 <- canMove space boxes rc' drc ]
  where
    rc' = (r+dr,c+dc)
    left = (r+dr,c+dc-1)
    right = (r+dr,c+dc+1)
run space boxes robot [] = boxes
run space boxes (rc@(r,c)) (mv:mvs)
  | null bs = run space boxes rc mvs
  | otherwise = run space boxes' rc' mvs
  where
    (drc@(dr,dc)) = toDir mv
    rc' = (r+dr,c+dc)
    bs = canMove space boxes rc drc
    [bs'] = bs
    nbs = S.fromList [ (r+dr,c+dc) | (r,c) <- S.toList bs' ]
    boxes' = S.union nbs $ boxes S.\\ bs'
gps (r,c) = 100 * r + c
everyOther [] = []
everyOther (x:_:rest) = x : everyOther rest
double (r,c) = [(r,c),(r,c+1)]
main = do
  lns <- lines <$> readFile "in/15.txt"
  let (m,(_:moves')) = break null lns
      moves = concat moves'
      space = S.fromList $ survey (/='#') m >>= double
      boxes = S.fromList $ survey (=='O') m >>= double
      [robot] = survey (=='@') m
      boxes' = run space boxes robot moves
  print $ sum $ map gps $ everyOther $ S.toList boxes' -- part 2

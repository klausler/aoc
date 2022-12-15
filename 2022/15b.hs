rip = map (\ch -> if ch >= '0' && ch <= '9' || ch == '-' then ch else ' ')
parse ln = let [a,b,c,d] = read <$> (words $ rip ln) in ((a,b),(c,d))
manhattan (x,y) (x',y') = abs (x-x') + abs (y-y')
overlap (sx,sy) d y
  | w < 0 = []
  | otherwise = [(sx-w, sx+w)]
  where w = d - abs (sy - y)
union ab [] = [ab]
union (ab@(a,b)) ((ab'@(a',b')):rest)
  | b < a' || b' < a = ab' : union ab rest
  | otherwise = union (min a a', max b b') rest
exclude sensors y = (,y) <$> (foldr union [] $ concat [ overlap sxy d y | (sxy,d) <- sensors ])
check [((a,b),_)] | a <= 0, b >= 4000000 = []
check [((1,b),y)] | b >= 4000000 = [(0,y)]
check [((a,3999999),y)] | a <= 0 = [(4000000,y)]
check [((a,b),y),((a',b'),_)]
  | a' == b + 2 = [(b+1,y)]
  | a == b' + 2 = [(b'+1,y)]
main = do
  input <- ((parse <$>) . lines) <$> readFile "in/15.txt"
  let sensors = [ (sxy, manhattan sxy bxy) | (sxy,bxy) <- input ]
      [(x,y)] = (exclude sensors <$> [0..4000000]) >>= check
  print $ 4000000 * x + y

import qualified Data.Set as S
parse ln = [ read ('(':w++")") | w <- words ln, head w /= '-' ] :: [(Int,Int)]
expand ((x,y):(rest@((x',y'):_)))
  | x==x', y==y' = expand rest
  | otherwise = (x,y) : expand ((x+signum(x'-x),y+signum(y'-y)) : rest)
expand last = last
descend maxY set (x,y)
  | y > maxY = (x,y)
  | S.notMember (x,y+1) set = descend maxY set (x,y+1)
  | S.notMember (x-1,y+1) set = descend maxY set (x-1,y+1)
  | S.notMember (x+1,y+1) set = descend maxY set (x+1,y+1)
  | otherwise = (x,y)
fillA maxY set
  | y > maxY = 0
  | otherwise = 1 + fillA maxY (S.insert (x,y) set)
  where (x,y) = descend maxY set (500,0)
fillB maxY set
  | xy == (500,0) = 1
  | otherwise = 1 + fillB maxY (S.insert xy set)
  where xy = descend maxY set (500,0)
main = do
  input <- ((parse <$>) . lines) <$> readFile "in/14.txt"
  let maxY = maximum $ snd <$> (concat input)
      set = S.fromList $ input >>= expand
  print $ fillA maxY set -- part A
  print $ fillB maxY set -- part B

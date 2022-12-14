import qualified Data.Set as S
parse ln = [ read ('(':w++")") | w <- words ln, head w /= '-' ] :: [(Int,Int)]
expand ((x,y):(rest@((x',y'):_)))
  | x==x', y==y' = expand rest
  | otherwise = (x,y) : expand ((x+signum(x'-x),y+signum(y'-y)) : rest)
expand last = last
descend maxY set (x,y)
  | y > maxY = Nothing
  | S.notMember (x,y+1) set = descend maxY set (x,y+1)
  | S.notMember (x-1,y+1) set = descend maxY set (x-1,y+1)
  | S.notMember (x+1,y+1) set = descend maxY set (x+1,y+1)
  | otherwise = Just (x,y)
fill maxY set
  | Just xy <- descend maxY set (500,0) = 1 + fill maxY (S.insert xy set)
  | otherwise = 0
main = do
  input <- ((parse <$>) . lines) <$> readFile "in/14.txt"
  let maxY = maximum $ snd <$> (concat input)
      set = S.fromList $ input >>= expand
  print $ fill maxY set

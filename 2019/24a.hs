import qualified Data.Set as S
neighbors (r,c) = [ (r-1,c), (r,c+1), (r+1,c), (r,c-1) ]
check set rc
  | ns == 1 = [rc]
  | not hasBug, ns == 2 = [rc]
  | otherwise = []
  where hasBug = S.member rc set
        ns = sum [ 1 | n <- neighbors rc, S.member n set ]
step set = S.fromList $ concat (check set <$> [ (r,c) | r <- [0..4], c <- [0..4] ])
bio set = sum [ 2^(5*r+c) | (r,c) <- S.toList set ]
findDup set (x:xs) = if S.member x set then x else findDup (S.insert x set) xs
main = do
  input <- lines <$> readFile "in/24.txt"
  let s0 = S.fromList [ (r,c) | (r,ln) <- zip [0..] input, (c,ch) <- zip [0..] ln, ch == '#' ]
  print $ findDup S.empty $ bio <$> iterate step s0 -- part A

import qualified Data.Set as S
neighbors (r,c,d) = up ++ right ++ down ++ left
  where up    | r == 0 = [ (1,2,d-1) ]
              | r == 3, c == 2 = [ (4,c',d+1) | c' <- [0..4] ]
              | otherwise = [ (r-1,c,d) ]
        right | c == 4 = [ (2,3,d-1) ]
              | r == 2, c == 1 = [ (r',0,d+1) | r' <- [0..4] ]
              | otherwise = [ (r,c+1,d) ]
        down  | r == 4 = [ (3,2,d-1) ]
              | r == 1, c == 2 = [ (0,c',d+1) | c' <- [0..4] ]
              | otherwise = [ (r+1,c,d) ]
        left  | c == 0 = [ (2,1,d-1) ]
              | r == 2, c == 3 = [ (r',4,d+1) | r' <- [0..4] ]
              | otherwise = [ (r,c-1,d) ]
check set (rcd@(r,c,d))
  | ns == 1 = [rcd]
  | not hasBug, ns == 2 = [rcd]
  | otherwise = []
  where hasBug = S.member rcd set
        ns = sum [ 1 | n <- neighbors rcd, S.member n set ]
halo set = [ (r,c,d) | r <- [0..4], c <- [0..4], r /= 2 || c /= 2
                     , d <- [minimum ds-1..maximum ds+1] ]
  where ds = [ d | (r,c,d) <- S.toList set ]
step set = S.fromList $ concat (check set <$> halo set)
bio set = sum [ 2^(5*r+c) | (r,c) <- S.toList set ]
findDup set (x:xs) = if S.member x set then x else findDup (S.insert x set) xs
main = do
  input <- lines <$> readFile "in/24.txt"
  let s0 = S.fromList [ (r,c,0) | (r,ln) <- zip [0..] input, (c,ch) <- zip [0..] ln, ch == '#' ]
  print $ S.size $ (iterate step s0) !! 200 -- part B

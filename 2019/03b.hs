import qualified Data.Map as M
import qualified Data.Set as S
parse ln = [ (d, read n) | (d:n) <- words $ (\ch->if ch==',' then ' ' else ch) <$> ln ]
to (r,c) ('R',n) m = ((r,c+n), M.fromList [ ((r,c+j),m+j) | j <- [1..n] ], n)
to (r,c) ('D',n) m = ((r+n,c), M.fromList [ ((r+j,c),m+j) | j <- [1..n] ], n)
to (r,c) ('L',n) m = ((r,c-n), M.fromList [ ((r,c-j),m+j) | j <- [1..n] ], n)
to (r,c) ('U',n) m = ((r-n,c), M.fromList [ ((r-j,c),m+j) | j <- [1..n] ], n)
visited m _ _ [] = m
visited m at n (step:rest) = let (at',m',n') = to at step n in visited (M.union m m') at' (n+n') rest
main = do
  input <- ((parse <$>) . lines) <$> readFile "in/03.txt"
  let trips = (visited M.empty (0,0) 0) <$> input
      hitsAt = S.toList $ foldr1 S.intersection $ (S.fromList . M.keys) <$> trips
  print $ minimum [ sum $ (M.! h) <$> trips | h <- hitsAt ]

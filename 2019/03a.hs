import Data.List(minimumBy)
import qualified Data.Set as S
parse ln = [ (d, read n) | (d:n) <- words $ (\ch->if ch==',' then ' ' else ch) <$> ln ]
to (r,c) ('R',n) = ((r,c+n), S.fromList [ (r,c+j) | j <- [0..n] ])
to (r,c) ('D',n) = ((r+n,c), S.fromList [ (r+j,c) | j <- [0..n] ])
to (r,c) ('L',n) = ((r,c-n), S.fromList [ (r,c-j) | j <- [0..n] ])
to (r,c) ('U',n) = ((r-n,c), S.fromList [ (r-j,c) | j <- [0..n] ])
visited set _ [] = set
visited set at (step:rest) = let (at',pts) = to at step in visited (S.union set pts) at' rest
manhattan (r,c) = abs r + abs c
manhattanCmp rc rc' = compare (manhattan rc) (manhattan rc')
main = do
  input <- ((parse <$>) . lines) <$> readFile "in/03.txt"
  let hits = foldr1 S.intersection $ (visited S.empty (0,0)) <$> input
  print $ manhattan $ minimumBy manhattanCmp $ hits S.\\ S.singleton (0,0)

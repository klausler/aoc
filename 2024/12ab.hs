import qualified Data.Map as M
import qualified Data.Set as S
dirs = [(-1,0),(0,1),(1,0),(0,-1)]
neighbors (r,c) = [ (r+dr,c+dc) | (dr,dc) <- dirs ]
remove rc set
  | rc `S.member` set = foldr remove (S.delete rc set) $ neighbors rc
  | otherwise = set
regions set
  | S.null set = []
  | otherwise = (set S.\\ set') : regions set'
  where set' = remove (head $ S.elems set) set
hasEdge reg (r,c) (dr,dc) = (r+dr,c+dc) `S.notMember` reg
p1cost reg = S.size reg * sum (edges <$> S.toList reg)
  where edges rc = sum [ 1 | d <- dirs, hasEdge reg rc d ]
p2cost reg = S.size reg * sum (edges <$> S.toList reg)
  where edges (rc@(r,c)) =
          fromEnum (hasEdge reg rc (-1,0) &&
            not ((r,c+1) `S.member` reg && hasEdge reg (r,c+1) (-1,0))) +
          fromEnum (hasEdge reg rc (0,1) &&
            not ((r+1,c) `S.member` reg && hasEdge reg (r+1,c) (0,1))) +
          fromEnum (hasEdge reg rc (1,0) &&
            not ((r,c+1) `S.member` reg && hasEdge reg (r,c+1) (1,0))) +
          fromEnum (hasEdge reg rc (0,-1) &&
            not ((r+1,c) `S.member` reg && hasEdge reg (r+1,c) (0,-1)))
main = do
  lns <- lines <$> readFile "in/12.txt"
  let sets = M.fromListWith S.union [ (ch, S.singleton (r,c)) | (r,ln) <- zip [1..] lns, (c,ch) <- zip [1..] ln ]
      regs = M.elems sets >>= regions
  print $ sum $ p1cost <$> regs -- part 1
  print $ sum $ p2cost <$> regs -- part 2

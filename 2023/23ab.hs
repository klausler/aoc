import qualified Data.Map as M
import qualified Data.Set as S
collapse graph end dist last at
  | at == end || length ns > 1 = [(at, dist)]
  | null ns = []
  | otherwise = collapse graph end (dist+1) at (head ns)
  where ns = filter (/=last) $ graph M.! at
explore graph end filled dist at
  | at == end = [dist]
  | otherwise = concat [ explore graph end (S.insert n filled) (dist+d) n
                       | (n,d) <- graph M.! at, n `S.notMember` filled ]
solve graph start end = maximum $ explore collapsed end S.empty 0 start
  where collapsed = M.fromList [ (rc, ns >>= collapse graph end 1 rc)
                               | (rc,ns) <- M.assocs graph
                               , rc == start || rc == end || length ns > 2 ]
main = do
  lns <- lines <$> readFile "in/23.txt"
  let (rows,columns) = (length lns, maximum $ length <$> lns)
      grid = M.fromList [ ((r,c), ch) | (r,ln) <- zip [0..] lns, (c,ch) <- zip [0..] ln, ch /= '#' ]
      isEmpty (r,c) = M.findWithDefault '#' (r,c) grid /= '#'
      (start, end) = (head $ M.keys grid, last $ M.keys grid)
      neighborsA (r,c) '^' = [(r-1,c)]; neighborsA (r,c) 'v' = [(r+1,c)]
      neighborsA (r,c) '<' = [(r,c-1)]; neighborsA (r,c) '>' = [(r,c+1)]
      neighborsA (r,c) '.' = [rc | rc<-[(r-1,c),(r+1,c),(r,c-1),(r,c+1)], isEmpty rc]
      graphA = M.mapWithKey neighborsA grid
  print $ solve graphA start end -- part A
  let neighborsB (r,c) _ = [rc | rc<-[(r-1,c),(r+1,c),(r,c-1),(r,c+1)], isEmpty rc]
      graphB = M.mapWithKey neighborsB grid
  print $ solve graphB start end -- part B

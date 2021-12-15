import Data.Array
import Data.Char(ord)
import Data.List(minimumBy)
import qualified Data.Map as M
import qualified Data.Set as S
type Coord = (Int,Int)
type Routes = M.Map Coord ([Coord], Int)
dijkstra :: (Coord -> Int) -> Coord -> Coord -> Routes -> Routes -> S.Set (Int,Int) -> Routes
dijkstra risk dest (at@(r,c)) routes frontier unvisited
  | at == dest = routes
  | otherwise = dijkstra risk dest next (M.insert next (nP,nD) routes) (M.delete next frontier') (next `S.delete` unvisited)
  where
    Just (pathToAt,atDist) = M.lookup at routes
    neighbors = filter (`S.member` unvisited) [ (r-1,c), (r,c-1), (r,c+1), (r+1,c) ]
    updateFrontier m [] = m
    updateFrontier m (n:ns)
      | Just (p,d) <- M.lookup n m, d < newDist = updateFrontier m ns
      | otherwise = updateFrontier (M.insert n (n:pathToAt, newDist) m) ns
      where newDist = atDist + risk n
    frontier' = updateFrontier frontier neighbors
    (next,(nP,nD)) = minimumBy cmp $ M.assocs frontier'
    cmp (_,(_,d1)) (_,(_,d2)) = compare d1 d2
doit arr rows cols mag = dest `M.lookup` routes
  where
    risk (r,c) = 1 + (v-1) `mod` 9
      where
        (rD,rM) = (r-1) `divMod` rows
        (cD,cM) = (c-1) `divMod` cols
        v = arr ! (rM+1,cM+1) + rD + cD
    dest = (mag * rows, mag * cols)
    unvisited = (1,1) `S.delete` (S.fromList [ (r,c) | r <- [1 .. (mag*rows)], c <- [1 .. (mag*cols)] ])
    routes = dijkstra risk dest (1,1) (M.fromList [((1,1),([(1,1)],0))]) M.empty unvisited
main = do
  lns <- lines <$> readFile "in/15.txt"
  let (rows,cols) = (length lns, length $ head lns)
      arr = listArray ((1,1),(rows,cols)) $ ((subtract (ord '0')) . ord) <$> concat lns
  print $ doit arr rows cols 1 -- part 1
  print $ doit arr rows cols 5 -- part 2

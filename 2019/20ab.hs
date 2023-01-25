import Data.Char(isUpper)
import Data.Maybe(maybeToList)
import qualified Data.Map as M
import qualified Data.Set as S
labNeighbors r c = [[(r-2,c),(r-1,c)],[(r,c-2),(r,c-1)],[(r,c+1),(r,c+2)],[(r+1,c),(r+2,c)]]
getLabel m r c = [ lab | ns <- labNeighbors r c, let lab = (\n->M.findWithDefault ' ' n m) <$> ns, all isUpper lab ]
realNeighbors atLoc r c = [ n | n <- [(r-1,c),(r,c+1),(r+1,c),(r,c-1)], M.findWithDefault ' ' n atLoc == '.' ]
allNeighbors atLoc jumps r c = realNeighbors atLoc r c ++ maybeToList (M.lookup (r,c) jumps)
bfs _ _ seen [] [] = error $ "stuck!"
bfs halo goal seen [] next = bfs halo goal seen next []
bfs halo goal seen (rP:rest) next
  | at == goal = length rP - 1
  | otherwise = bfs halo goal seen' rest next'
  where at = head rP
        new = filter (`S.notMember` seen) $ halo at
        seen' = S.union seen $ S.fromList new
        next' = ((:rP) <$> new) ++ next
main = do
  input <- lines <$> readFile "in/20.txt"
  let atLoc = M.fromList [ ((r,c),ch) | (r,ln) <- zip [0..] input, (c,ch) <- zip [0..] ln, ch == '.' || isUpper ch ]
      labLoc = M.fromListWith (++) [ (lab,[(r,c)]) | ((r,c),ch) <- M.assocs atLoc, ch == '.', lab <- getLabel atLoc r c ]
      jumpMap = M.fromList $ concat [ [(x,y),(y,x)] | (_,pts) <- M.assocs labLoc, length pts == 2, let [x,y] = pts ]
      graph = M.fromList [ ((r,c), allNeighbors atLoc jumpMap r c) | ((r,c),ch) <- M.assocs atLoc, ch == '.' ]
      finish = head $ labLoc M.! "ZZ"
      startA = head $ labLoc M.! "AA" :: (Int,Int)
      startB = (fst startA, snd startA, 0)
      (rs,cs) = unzip $ [ (r,c) | ((r,c),ch) <- M.assocs atLoc, ch == '.' ]
      [minR,minC,maxR,maxC] = [minimum, maximum] <*> [rs,cs]
      isOuter r c = r == minR || r == maxR || c == minC || c == maxC
      haloB (r,c,d) = rNs ++ jumpsDown ++ jumpsUp
        where rNs = [ (r',c',d) | (r',c') <- realNeighbors atLoc r c ]
              jmps = maybeToList $ M.lookup (r,c) jumpMap
              jumpsDown = [ (r',c',d-1) | d > 0, isOuter r c, (r',c') <- jmps ]
              jumpsUp = [ (r',c',d+1) | not $ isOuter r c, (r',c') <- jmps ]
  print $ bfs (graph M.!) finish (S.singleton startA) [] [[startA]] -- part A
  print $ bfs haloB (fst finish, snd finish, 0) (S.singleton startB) [] [[startB]] -- part B

import qualified Data.Map as M
import qualified Data.Set as S
edgesLeaving graph nodes =
  [ (a,b) | a <- S.toList nodes, b <- S.toList $ graph M.! a
          , b `S.notMember` nodes ]
frontier graph set = S.fromList $ map snd $ edgesLeaving graph set
-- expand a set to include all nodes within maxDist steps
local graph 0 set = set
local graph maxDist set = local graph (maxDist-1) $ set `S.union` frontier graph set
-- expand a set of nodes with all nodes whose neighbors are all in set
closure graph set
  | null more = set
  | otherwise = closure graph $ S.union set $ S.fromList more
  where more = [ v | v <- S.toList $ M.keysSet graph S.\\ set
                   , (graph M.! v) `S.isSubsetOf` set]
reachable graph set
  | null more = set
  | otherwise = reachable graph $ set `S.union` more
  where more = frontier graph set
remains graph exclude = M.fromList [ (v, vns S.\\ exclude) | (v,vns) <- M.assocs graph, v `S.notMember` exclude ]
main = do
  lns <- lines <$> readFile "in/25.txt"
  let graph = M.fromListWith S.union $ concat
              [ [(x, S.singleton y), (y, S.singleton x)]
              | ln <- lns, let (x':ys) = words ln, let x = init x', y <- ys ]
      nodes = M.size graph
  print $ head [ S.size s * S.size crf | maxDist <- [2..10], v <- M.keys graph
               , let s = closure graph $ local graph maxDist $ S.singleton v
               , let els = edgesLeaving graph s, length els == 3
               , let f = S.fromList $ map snd els
               , let crf = closure graph $ reachable (remains graph s) f
               , S.size s + S.size crf == nodes
               , length (edgesLeaving graph crf) == 3 ]

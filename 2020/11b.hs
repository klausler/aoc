import Data.Maybe(maybeToList)
import qualified Data.Map as M
dirs = [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]
walk m s (dr,dc) = step s
  where
    step (r,c) = case M.lookup ns m of
        Just '#' -> 1
        Just 'L' -> 0
        Just '.' -> step ns
        Nothing -> 0
      where
         ns = (r+dr,c+dc)
numAdj m s = sum $ map (walk m s) dirs
seat m (s,c)
  | c == 'L' && n == 0 = (s,'#')
  | c == '#' && n >= 5 = (s,'L')
  | otherwise = (s,c)
  where
    n = numAdj m s
iter m = M.fromList $ map (seat m) $ M.assocs m
fixPoint f m = let m' = f m in if m == m' then m else fixPoint f m'
main = do
  txt <- readFile "input11.txt"
  let
    f (r,ln) = [ ((r,c),ch) | (c,ch) <- zip [0..] ln ]
    m = M.fromList $ f =<< (zip [0..] $ lines txt)
  print $ length $ filter (=='#') $ M.elems $ fixPoint iter m

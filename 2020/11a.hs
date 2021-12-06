import Data.Maybe(maybeToList)
import qualified Data.Map as M
around (r,c) = [(r-1,c-1), (r-1,c), (r-1,c+1), (r,c-1), (r,c+1), (r+1,c-1), (r+1,c), (r+1,c+1)]
numAdj m s = length $ filter (=='#') $ around s >>= (maybeToList.(`M.lookup` m))
seat m (s,c)
  | c == 'L' && n == 0 = (s,'#')
  | c == '#' && n >= 4 = (s,'L')
  | otherwise = (s,c)
  where
    n = numAdj m s
iter m = M.fromList $ map (seat m) $ M.assocs m
fixPoint f m = let m' = f m in if m == m' then m else fixPoint f m'
main = do
  txt <- readFile "in/11.txt"
  let
    f (r,ln) = [ ((r,c),ch) | (c,ch) <- zip [0..] ln ]
    m = M.fromList $ f =<< (zip [0..] $ lines txt)
  print $ length $ filter (=='#') $ M.elems $ fixPoint iter m

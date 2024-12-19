import Data.Char(ord)
import qualified Data.Map as M
import qualified Data.Set as S
dirs=[(-1,0),(0,1),(1,0),(0,-1)]
descend m (-1) paths = paths
descend m h paths = descend m (h-1) $ foldr f paths $ m M.! h
  where
    higher = S.fromList $ m M.! (h+1)
    f (rc@(r,c)) rs = foldr (g rc) rs [ uprc | (dr,dc) <- dirs, let uprc = (r+dr,c+dc), uprc `S.member` higher ]
    g rc uprc rs = M.insertWith (+) rc (M.findWithDefault 0 uprc rs) rs
main = do
  lns <- lines <$> readFile "in/10.txt"
  let m = M.fromListWith (++) [ (ord ch - ord '0', [(r,c)]) | (r,ln) <- zip [1..] lns, (c,ch) <- zip [1..] ln ]
      paths = descend m 8 $ M.fromList [ (rc, 1) | rc <- m M.! 9 ]
  print $ sum $ (paths M.!) <$> (m M.! 0) -- part 2

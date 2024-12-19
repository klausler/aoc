import Data.Char(ord)
import qualified Data.Map as M
import qualified Data.Set as S
dirs=[(-1,0),(0,1),(1,0),(0,-1)]
descend m (-1) reaches = reaches
descend m h reaches = descend m (h-1) $ foldr f reaches $ m M.! h
  where
    higher = S.fromList $ m M.! (h+1)
    f (rc@(r,c)) rs = foldr (g rc) rs [ uprc | (dr,dc) <- dirs, let uprc = (r+dr,c+dc), uprc `S.member` higher ]
    g rc uprc rs = M.insertWith S.union rc (M.findWithDefault S.empty uprc rs) rs
main = do
  lns <- lines <$> readFile "in/10.txt"
  let m = M.fromListWith (++) [ (ord ch - ord '0', [(r,c)]) | (r,ln) <- zip [1..] lns, (c,ch) <- zip [1..] ln ]
      reaches = descend m 8 $ M.fromList [ (rc, S.singleton rc) | rc <- m M.! 9 ]
  print $ sum $ (S.size . (reaches M.!)) <$> (m M.! 0) -- part 1

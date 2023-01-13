import Data.Ratio
import Data.List(maximumBy, minimumBy)
import qualified Data.Map as M
import qualified Data.Set as S
angle (r,c) (r',c') -- (quadrant, rational tangent)
  | r' < r, c' >= c = (0, (c'-c) % (r-r'))
  | r' >= r, c' > c = (1, (r'-r) % (c'-c))
  | r' > r, c' <= c = (2, (c-c') % (r'-r))
  | r' <= r, c' < c = (3, (r-r') % (c-c'))
  | otherwise = error $ show ((r,c),(r',c'))
((.*.) `on` f) x y = f x .*. f y
visible points (rc@(r,c)) = M.fromListWith S.union [ (angle rc p, S.singleton p) | p <- points, p /= rc ]
zaps rc m [] = if M.null m then [] else zaps rc m $ M.assocs m
zaps (rc@(r,c)) m ((angle,set):rest) = hit : zaps rc m' rest
  where hit = minimumBy (compare `on` distSq) $ S.toList set
        set' = set S.\\ S.singleton hit
        m' = if S.null set' then M.delete angle m else M.insert angle set' m
        distSq (r',c') = (r-r')^2 - (c-c')^2
main = do
  input <- lines <$> readFile "in/10.txt"
  let points = [ (r,c) | (r,ln) <- zip [0..] input, (c,ch) <- zip [0..] ln, ch == '#' ]
      (best,m) = maximumBy (compare `on` (M.size . snd)) [ (p, visible points p) | p <- points ]
      (r,c) = zaps best m [] !! 199
  print $ M.size m -- part A
  print $ r + 100 * c -- part B

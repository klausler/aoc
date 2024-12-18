import qualified Data.Map as M
import qualified Data.Set as S
pairs [_] = []
pairs (x:ys) = [(x,y)|y<-ys] ++ pairs ys
isOk rows cols (r,c) = r >= 1 && r <= rows && c >= 1 && c <= cols
part1 ok (xrc@(xr,xc)) (yrc@(yr,yc)) = filter ok $ s1 ++ s2
  where
    (dr,dc) = (xr-yr, xc-yc)
    (dr2,dc2) = (dr `div` 2, dc `div` 2)
    s1 = [ (xr+dr,xc+dc), (yr-dr,yc-dc) ]
    s2 | even dr, even dc = [ (xr-dr2, xc-dc2), (yr+dr2,yc+dc2) ]
       | otherwise = []
part2 m ok (xrc@(xr,xc)) (yrc@(yr,yc)) = filter ok $ [ (xr+t*dr,xc+t*dc) | t<-[-m..m] ]
  where (dr,dc) = (xr-yr, xc-yc)
anSet f set = S.fromList $ (pairs $ S.toList set) >>= uncurry f
main = do
  lns <- lines <$> readFile "in/08.txt"
  let stationMap = M.fromListWith S.union
        [ (ch, S.singleton (r,c))
        | (r,ln) <- zip [1..] lns, (c,ch) <- zip [1..] ln, ch /= '.' ]
      (rows, cols) = (length lns, maximum $ length <$> lns)
  print $ S.size $ S.unions $ ((anSet $ part1 $ isOk rows cols) <$> M.elems stationMap) -- part 1
  print $ S.size $ S.unions $ ((anSet $ part2 (rows `max` cols) $ isOk rows cols) <$> M.elems stationMap) -- part 2

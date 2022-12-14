import Data.Char(ord)
import qualified Data.Map as M
import qualified Data.Set as S
val 'S' = val 'a'
val 'E' = val 'z'
val ch = ord ch - ord 'a'
canStep map (x,y) = [ pt | pt <- [ (x-1,y), (x+1,y), (x,y-1), (x,y+1) ]
                         , M.member pt map, map M.! pt <= limit ]
  where limit = 1 + (map M.! (x,y))
rounds map already last = next : rounds map (S.union already next) next
  where next = S.fromList [ np | lp <- S.toList last, np <- canStep map lp ] S.\\ already
steps map end starts = 1 + (length $ takeWhile (S.notMember end) $ rounds map starts starts)
main = do
  lns <- lines <$> readFile "in/12.txt"
  let map0 = M.fromList [ ((x,y), ch) | (y,ln) <- zip [0..] lns, (x,ch) <- zip [0..] ln ]
      [startA] = [ xy | (xy,'S') <- M.assocs map0 ]
      startsB = [ xy | (xy,ch) <- M.assocs map0, val ch == 0 ]
      [end] = [ xy | (xy,'E') <- M.assocs map0 ]
      map = M.fromList [ (xy, val ch) | (xy,ch) <- M.assocs map0 ]
  print $ steps map end $ S.singleton startA -- part A
  print $ steps map end $ S.fromList startsB -- part B

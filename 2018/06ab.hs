import Data.List(groupBy,sortBy)
import qualified Data.Map as M
import qualified Data.Set as S
((.*.) `on` f) x y = f x .*. f y
manhattan (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)
nearests xy = head . groupBy ((==) `on` manhattan xy) . sortBy (compare `on` manhattan xy)
sumDists xys xy = sum $ manhattan xy <$> xys
findCorner f (x,y) (dx,dy)
  | f (x,y) = (x,y)
  | otherwise = findCorner f (x+dx,y+dy) (dx,dy)
main = do
  input <- lines <$> readFile "in/06.txt"
  let xys = [ read $ '(' : (xy ++ ")") | xy <- input ]
      (xs, ys) = unzip xys
      [minX,minY,maxX,maxY] = [minimum, maximum] <*> [xs, ys]
      nMap = M.fromList [ ((x,y),nearests (x,y) xys) | x <- [minX-1..maxX+1], y <- [minY-1..maxY+1] ]
      inf = S.fromList $ concat [ ns | ((x,y),ns) <- M.assocs nMap, x < minX || x > maxX || y < minY || y > maxY ]
      counts = M.fromListWith (+) [ (n,1) | ((x,y),ns) <- M.assocs nMap, length ns == 1, n <- ns, S.notMember n inf ]
      isClose = (<10000) . sumDists xys
      cf = findCorner (not . isClose)
      (xs',ys') = unzip [ cf (minX,minY) (-1,-1), cf (maxX,minY) (1,-1)
                        , cf (maxX,maxY) (1,1), cf (minX,maxY) (-1,1) ]
      [minX',minY',maxX',maxY'] = [minimum, maximum] <*> [xs',ys']
  print $ maximum $ M.elems counts -- part A
  print $ length [() | x <- [minX'..maxX'], y <- [minY'..maxY'], sumDists xys (x,y) < 10000 ] -- part B

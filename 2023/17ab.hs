import Data.Array
import qualified Data.Set as S
search arr rows cols minD maxD = loop
  where
    loop set
      | at == (rows,cols), lastDirCt >= minD = d
      | otherwise = loop $ S.union set' $ S.fromList nexts
      where
        (((d,(at@(r,c)),(lastDir@(ldr,ldc)),lastDirCt)), set') = S.deleteFindMin set
        nexts = [ (d + arr ! at', at', dir, if dir == lastDir then lastDirCt + 1 else 1)
                | (dir@(dr,dc)) <- if lastDirCt < minD then [lastDir] else [(1,0), (-1,0), (0,1), (0,-1)]
                , dir /= (-ldr,-ldc), dir /= lastDir || lastDirCt < maxD
                , let (at'@(r',c')) = (r+dr,c+dc), r' >= 1, r' <= rows, c' >= 1, c' <= cols ]
main = do
  lns <- lines <$> readFile "in/17.txt"
  let (rows, cols) = (length lns, maximum $ length <$> lns)
      arr = listArray ((1,1),(rows,cols)) $ fmap (read . (:"")) $ concat lns
  print $ search arr rows cols 0 3 $ S.singleton (0,(1,1),(0,0),0) -- part A
  print $ search arr rows cols 4 10 $ S.fromList [(0,(1,1),(1,0),0), (0,(1,1),(0,1),0)] -- part B

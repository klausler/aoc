import qualified Data.Map as M
part1 = [ (yd,xd) | yd <- [-1,0,1], xd <- [-1,0,1], not (yd==0 && xd==0) ]
part2 = [ (yd,xd) | yd <- [-1,1], xd <- [-1,1] ]
explore dirs keep start m y x = (take keep . explore' start) <$> dirs
  where explore' n (dir@(yd,xd))
          | Just ch <- M.lookup (y+n*yd,x+n*xd) m = ch : explore' (n+1) dir
          | otherwise = ""
main = do
  lns <- lines <$> readFile "in/04.txt"
  let m = M.fromList [ ((y,x),ch) | (y,ln) <- zip [1..] lns, (x,ch) <- zip [1..] ln ]
  print $ sum [ 1 | (y,x) <- M.keys m, str <- explore part1 4 0 m y x, str == "XMAS" ] -- part 1
  print $ sum [ 1 | (y,x) <- M.keys m, (length $ filter (=="MAS") (explore part2 3 (-1) m y x)) == 2 ] -- part 2

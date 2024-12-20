import qualified Data.Set as S
strip ch
  | ch == '-' = '-'
  | ch >= '0', ch <= '9' = ch
  | otherwise = ' '
predict n [px,py,vx,vy] = (px',py')
  where
    px' = (px + n * vx) `mod` 101
    py' = (py + n * vy) `mod` 103
future pvs n = S.fromList $ predict n <$> pvs
isTriangleTip set (x,y) = all (`S.member` set)
  [ (x+dx,y+dy) | dy<-[0..3], dx<-[-dy..dy] ]
hasTriangle set = any (isTriangleTip set) $ S.toList set
solutions pvs = filter (hasTriangle . future pvs) [0..]
draw set = [ [ if (x,y) `S.member` set then '#' else '.' | x <- [0..100 ] ] | y <- [0..102] ]
emit pvs n = do
  mapM_ putStrLn $ draw $ future pvs n
  print n
main = do
  lns <- lines <$> readFile "in/14.txt"
  let pvs = (map read . words . map strip) <$> lns
  emit pvs $ head $ solutions pvs

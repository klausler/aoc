import Data.List(group,sort)
strip ch
  | ch == '-' = '-'
  | ch >= '0', ch <= '9' = ch
  | otherwise = ' '
quad (x,y)
  | x == 50 = 0
  | y == 51 = 0
  | otherwise = 1 + (if x > 50 then 1 else 0)
                  + (if y > 50 then 2 else 0)
predict n [px,py,vx,vy] = (px',py')
  where
    px' = (px + 100 * vx) `mod` 101
    py' = (py + 100 * vy) `mod` 103
main = do
  lns <- lines <$> readFile "in/14.txt"
  let pvs = (map read . words . map strip) <$> lns
  print $ product $ map length $ tail $ group $ sort $ (quad . predict 100) <$> pvs -- part 1

import Data.Ratio
strip ch | ch >= '0' && ch <= '9' = ch
strip _ = ' '
groupsOf :: Int -> [a] -> [[a]]
groupsOf n [] = []
groupsOf n xs = let (p,s) = splitAt n xs in p : groupsOf n s
-- x1*a+x2*b = x3, y1*a+y2*b = y3
-- t1 = y1/x1, then (y2-t1*x2)*b = y3 - t1*x3
-- so b = (y3 - t1*x3) / (y2 - t1*x2)
-- then a = (x3 - x2*b) / x1
play :: [Rational] -> Rational
play [x1,y1,x2,y2,x3,y3]
  | denominator b == 1, denominator a == 1 = 3 * a + b
  | otherwise = 0
  where
    t1 = y1 / x1
    b = (y3 - t1 * x3) / (y2 - t1 * x2)
    a = (x3 - x2 * b) / x1
part2 [x1,y1,x2,y2,x3,y3] = [x1,y1,x2,y2,x3+10000000000000,y3+10000000000000]
main = do
  input <- map strip <$> readFile "in/13.txt"
  let vals = read <$> words input :: [Integer]
      machs = groupsOf 6 $ ((% 1) <$> vals) :: [[Rational]]
  print $ sum $ play <$> machs -- part 1
  print $ sum $ (play . part2) <$> machs -- part 2

f ln = let [(cmd:_), num] = words ln in (cmd, read num)
cmd (d,h,a) ('f', n) = (d + a * n `max` 0, h + n, a)
cmd (d,h,a) ('u', n) = (d, h, a - n)
cmd (d,h,a) ('d', n) = (d, h, a + n)
main = do
  lns <- lines <$> readFile "in/02.txt"
  let (d,h,_) = foldl cmd (0,0,0) $ f <$> lns
  print $ d*h

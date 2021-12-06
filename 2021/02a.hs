f ln = let [(cmd:_), num] = words ln in (cmd, read num)
cmd (d,h) ('f', n) = (d, h + n)
cmd (d,h) ('u', n) = (d - n `max` 0, h)
cmd (d,h) ('d', n) = (d + n, h)
main = do
  lns <- lines <$> readFile "in/02.txt"
  print $ uncurry (*) $ foldl cmd (0,0) $ f <$> lns

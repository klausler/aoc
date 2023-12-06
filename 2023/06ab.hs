numOver t d = sum [ 1 | ct <- [0..t], ct * (t-ct) > d ]
main = do
  lns <- lines <$> readFile "in/06.txt"
  let [ts,ds] = (fmap read . tail . words) <$> lns
      [tB,dB] = fmap read $ (concat . tail . words) <$> lns
  print $ product $ zipWith numOver ts ds -- part A
  print $ numOver tB dB -- part B

play rolls p1At p1Score p2At p2Score (r1:r2:r3:rs)
  | p1Score' >= 1000 = (rolls + 3, p2Score)
  | otherwise = play (rolls + 3) p2At p2Score p1At' p1Score' rs
  where
    p1At' = 1 + ((p1At + r1 + r2 + r3 - 1) `mod` 10)
    p1Score' = p1Score + p1At'
main = do
  lns <- lines <$> readFile "in/21.txt"
  let [p1At,p2At] = (read . last . words) <$> lns
      (rolls, loser) = play 0 p1At 0 p2At 0 $ cycle [1..100]
  print $ rolls * loser

import Data.Array
rolls = [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)]
pairSum (a,b) (c,d) = (a+c, b+d)
advance at rollSum = 1 + ((at + rollSum - 1) `mod` 10)
dpArray = listArray ((1,0,1,0),(10,20,10,20)) [ dpFunc p1At p1Score p2At p2Score | p1At <- [1..10], p1Score <- [0..20], p2At <- [1..10], p2Score <- [0..20] ]
dpFunc p1At p1Score p2At p2Score = foldr1 pairSum [ futures (advance p1At rollSum) ct | (rollSum, ct) <- rolls ]
  where
    futures p1At' ct
      | p1Score' >= 21 = (ct, 0)
      | otherwise = (ct * p1f, ct * p2f)
      where
        p1Score' = p1Score + p1At'
        (p2f, p1f) = dpArray ! (p2At, p2Score, p1At', p1Score')
main = do
  lns <- lines <$> readFile "in/21.txt"
  let [p1At,p2At] = (read . last . words) <$> lns
      (p1Unis,p2Unis) = dpArray ! (p1At, 0, p2At, 0)
  print $ p1Unis `max` p2Unis

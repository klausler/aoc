parse "" = []
parse ln = let (p,s) = break (`elem` ":;") ln in p : parse (drop 1 s)
counts trial = f (0,0,0) $ words trial
  where f cts [] = cts
        f (r,g,b) (n:"red":rest) = f (read n,g,b) rest
        f (r,g,b) (n:"green":rest) = f (r,read n,b) rest
        f (r,g,b) (n:"blue":rest) = f (r,g,read n) rest
isPossible (r,g,b) = r <= 12 && g <= 13 && b <= 14
score trials = let (rs,gs,bs) = unzip3 trials in maximum rs * maximum gs * maximum bs
main = do
  input <- (lines . filter (/=','))  <$> readFile "in/02.txt"
  let games = (fmap counts . drop 1 . parse) <$> input
  print $ sum [ n | (n,trials) <- zip [1..] games, all isPossible trials ] -- part A
  print $ sum $ score <$> games -- part B

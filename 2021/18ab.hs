import Data.Char(ord)
transform 0 "" = []
transform d ('[':cs) = transform (d+1) cs
transform d (']':cs) = transform (d-1) cs
transform d (',':cs) = transform d cs
transform d (n:cs) = (d, ord n - ord '0') : transform d cs
toFix f x = let x' = f x in if x == x' then x else toFix f x'
post [] = []
post ((ad,a):(5,b):(5,c):(dd,d):rest) | ad < 5 = (ad,(a+b)):(4,0):(dd,c+d):rest
post ((5,_):(5,c):(dd,d):rest) = (4,0):(dd,c+d):rest
post ((ad,a):(5,b):(5,_):[]) = (ad,(a+b)):(4,0):[]
post ((d,n):rest) | n > 9 = let n' = n `div` 2 in (d+1,n'):(d+1,n-n'):rest
post ((d,n):rest) | d < 5, n < 10 = (d,n) : post rest
post x = error $ show x
add x y = toFix post $ deepen <$> (x ++ y)
  where deepen (d,n) = (d+1,n)
dmag depth list@((d,n):rest)
  | d == depth = (n,rest)
  | otherwise = let (m1,r1) = dmag (depth+1) list
                    (m2,r2) = dmag (depth+1) r1 in (3*m1+2*m2, r2)
mag = fst . dmag 0
sumPairs [] = []
sumPairs (a:rest) = concat [ [ add a b, add b a ] | b <- rest ] ++ sumPairs rest
main = do
  nums <- (transform 0 <$>) <$> lines <$> readFile "in/18.txt"
  print $ mag $ foldl1 add nums -- part 1
  print $ maximum $ mag <$> sumPairs nums -- part 2

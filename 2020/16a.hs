import Control.Applicative((<*>))
rule r n = rf rng1 n || rf rng2 n
  where
    (tag,':':r') = break (':'==) r
    [rng1,"or",rng2] = words r'
    rf rng = test
      where
        (a,'-':b) = break ('-'==) rng
        (a',b') = (read a, read b) :: (Int,Int)
        test n = n >= a' && n <= b'
vals = map read . words . map (\c -> if c == ',' then ' ' else c)
noRule rs n = not $ or $ rs <*> [n]
main = do
  txt <- readFile "in/16.txt"
  let
    (rules,"":"your ticket:":my:"":"nearby tickets:":nearby) = span (not.null) $ lines txt
    rs = map rule rules
  print $ sum $ filter (noRule rs) $ concatMap vals nearby

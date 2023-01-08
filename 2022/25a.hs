import Data.Char(ord,chr)
dVal '=' = -2
dVal '-' = -1
dVal ch = ord ch - ord '0'
valCh (-2) = '='
valCh (-1) = '-'
valCh n = chr $ ord '0' + n
fromBal5 str = sum $ zipWith (*) (iterate (5*) 1) $ dVal <$> reverse str
toBal5 0 = "0"
toBal5 n = reverse $ toRevBal5 n
toRevBal5 0 = ""
toRevBal5 n
  | m > 2 = valCh (m - 5) : toRevBal5 (d + 1)
  | m < -2 = valCh (m + 5) : toRevBal5 (d - 1)
  | otherwise = valCh m : toRevBal5 d
  where (d,m) = n `divMod` 5
main = do
  lns <- lines <$> readFile "in/25.txt"
  putStrLn $ toBal5 $ sum $ fromBal5 <$> lns -- part A

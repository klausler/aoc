import Data.List(partition,transpose)
import qualified Data.Map as M
gravity "" = ""
gravity ('#':rest) = '#' : gravity rest
gravity xs = rocks ++ dots ++ gravity xs'
  where (got,xs') = span (/='#') xs
        (rocks,dots) = partition (=='O') got
score xs = sum [ at | (at,ch) <- zip [1..] $ reverse xs, ch == 'O' ]
doCycle lns = tiltEast ts
  where tn = transpose $ gravity <$> transpose lns
        tw = gravity <$> tn
        ts = transpose $ tiltEast $ transpose tw
        tiltEast = map (reverse . gravity . reverse)
findLoop at map (x:xs)
  | Just first <- M.lookup x map = (first,at-first)
  | otherwise = findLoop (at+1) (M.insert x at map) xs
main = do
  lns <- lines <$> readFile "in/14.txt"
  print $ sum $ score <$> gravity <$> transpose lns -- part A
  let cycles = iterate doCycle lns
      (start,lpLength) = findLoop 0 M.empty cycles
      reduced = start + (1000000000 - start) `mod` lpLength
  print $ sum $ score <$> transpose (cycles !! reduced) -- part B

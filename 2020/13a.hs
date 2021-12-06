import Data.Function(on)
import Data.List(minimumBy)
main = do
  txt <- readFile "input13.txt"
  let
    [l1,l2] = lines txt
    now = read l1
    buses = map read $ filter (/="x") $ words $ map (\c -> if c == ',' then ' ' else c) l2
    wait freq = freq * (now `div` freq) + freq - now
    (b,w) = minimumBy (compare `on` snd) [ (b, wait b) | b <- buses ]
  print $ (b,w,b*w)

import Data.List(sort)
count n = length . filter (==n)
main = do
  txt <- readFile "in/10.txt"
  let
    nums = map read $ lines txt
    sorted = sort nums
    diffs = zipWith (-) (sorted ++ [last sorted + 3]) (0:sorted)
  print $ count 1 diffs * count 3 diffs

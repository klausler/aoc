import Data.List(sort, tails)
fib3List = [1, 1, 2] ++ map (sum . take 3) (tails fib3List)
f [] (n:_) = n
f (1:xs) (_:ns) = f xs ns
f (3:xs) (n:_) = n * f xs fib3List
main = do
  txt <- readFile "input10.txt"
  let
    sorted = sort $ map read $ lines txt
    diffs = zipWith (-) (sorted ++ [last sorted + 3]) (0:sorted)
  print $ f diffs fib3List

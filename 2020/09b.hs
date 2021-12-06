import Data.List(tails)
sums []  = []
sums (x:xs) = map (x+) xs ++ sums xs
search set (x:xs) = if x `elem` sums set then search (tail set ++ [x]) xs else x
contig 0 _ = [[]]
contig _ [] = []
contig n (x:xs) = if n < x then [] else map (x:) $ contig (n-x) xs
main = do
  txt <- readFile "in/09.txt"
  let
    nums = map read $ lines txt
    target = uncurry search $ splitAt 25 nums
  print [ minimum c + maximum c | t <- tails nums, c <- contig target t, not $ null $ tail c ]

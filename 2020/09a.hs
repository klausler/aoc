sums []  = []
sums (x:xs) = map (x+) xs ++ sums xs
search set (x:xs)
  | x `elem` sums set = search (tail set ++ [x]) xs
  | otherwise = x
main = do
  txt <- readFile "input09.txt"
  let
    (set,xs) = splitAt 25 $ map read $ lines txt
  print $ search set xs

diffs xs = zipWith subtract (tail xs) xs
partA [] = 0
partA xs = last xs + partA (diffs xs)
partB [] = 0
partB xs = head xs - partB (diffs xs)
main = do
  lns <- lines <$> readFile "in/09.txt"
  let hists = map (map read . words) lns
  print $ sum $ map partA hists -- part A
  print $ sum $ map partB hists -- part B

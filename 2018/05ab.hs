import Data.Char(isLower,isUpper,toLower)
react (x:(rest@(y:ys)))
  | isLower x, isUpper y, x == toLower y = react ys
  | isUpper x, isLower y, y == toLower x = react ys
  | otherwise = x : react rest
react xs = xs
reduce xs = if xs == xs' then xs else reduce xs'
  where xs' = react xs
remove x = filter ((/=x) . toLower)
removed xs = [ length $ reduce $ remove letter xs | letter <- ['a'..'z'] ]
main = do
  reduced <- (reduce . init) <$> readFile "in/05.txt"
  print $ length reduced -- part A
  print $ minimum $ removed reduced -- part B

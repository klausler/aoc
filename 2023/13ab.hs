import Data.List(transpose)
chunked [] = []
chunked ("":rest) = chunked rest
chunked lns = let (chunk, rest) = break null lns in chunk : chunked rest
countDiffs xs ys = sum $ fromEnum <$> zipWith (/=) xs ys
findReflection diffs [] (ln:lns) = findReflection diffs [ln] lns
findReflection _ _ [] = Nothing
findReflection diffs reversed (rest@(x:xs))
  | diffs == (sum $ zipWith countDiffs reversed rest) = Just $ length reversed
  | otherwise = findReflection diffs (x:reversed) xs
score diffs image
  | Just horizontal <- findReflection diffs [] image = 100 * horizontal
  | Just vertical <- findReflection diffs [] $ transpose image = vertical
main = do
  images <- (chunked . lines) <$> readFile "in/13.txt"
  print $ sum $ score 0 <$> images -- part A
  print $ sum $ score 1 <$> images -- part B

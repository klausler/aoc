import Data.List(elemIndex, sort)
fromJust (Just x) = x
pairCh '(' = Just ')'
pairCh '[' = Just ']'
pairCh '{' = Just '}'
pairCh '<' = Just '>'
pairCh _ = Nothing
squish "" = ""
squish (a:as)
  | Just a' <- pairCh a, (a'':as'') <- as', a' == a'' = as''
  | otherwise = a : as'
  where as' = squish as
part1Score (')':_) = 3
part1Score (']':_) = 57
part1Score ('}':_) = 1197
part1Score ('>':_) = 25137
part1Score (_:rest) = part1Score rest
part1Score "" = 0
part2Score n "" = n
part2Score n (c:cs) = part2Score (5*n + 1 + fromJust (c `elemIndex` "([{<")) cs
main = do
  lns <- lines <$> readFile "in/10.txt"
  let squished = squish <$> lns
      incomp = filter ((==0) . part1Score) squished
  print $ sum $ part1Score <$> squished -- part 1
  print $ (sort $ part2Score 0 <$> reverse <$> incomp) !! (length incomp `div` 2) -- part 2

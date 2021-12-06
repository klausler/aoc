group ('\n':x@('\n':_)) = '\n':group x
group ('\n':x) = group x
group (x:xs) = x:group xs
group "" = ""

distinct (x:xs)
  | x `elem` xs = distinct xs
  | otherwise = x:distinct xs
distinct [] = []

main = fmap (sum . map (length.distinct) . lines . group)  (readFile "in/06.txt") >>= print

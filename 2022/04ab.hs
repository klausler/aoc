depunct '-' = ' '
depunct ',' = ' '
depunct x = x
contains [a, b, c, d] = c>=a && d<=b || a>=c && b<=d
overlap [a, b, c, d] = not (c>b || a>d)
main = do
  input <- lines <$> readFile "in/04.txt"
  let vals = ((read <$>) . words . map depunct) <$> input :: [[Int]]
  print $ length $ filter contains vals -- part A
  print $ length $ filter overlap vals -- part B


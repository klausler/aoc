scan sz at str
  | sum [ 1 | match <- (==) <$> block <*> block, match ] == sz = at + sz
  | otherwise = scan sz (at+1) $ tail str
  where block = take sz str
main = do
  input <- readFile "in/06.txt"
  print $ scan 4 0 input -- part A
  print $ scan 14 0 input -- part B

cmp xs ys = (sum $ fromEnum <$> zipWith (/=) xs ys) == 1
main = do
  input <- lines <$> readFile "in/02.txt"
  let ((xs,ys):_) = [ (xs,ys) | xs <- input, ys <- input, cmp xs ys ]
  putStrLn [ x | (x,y) <- zip xs ys, x == y ] -- part B

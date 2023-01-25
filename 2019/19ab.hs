import IntCode
solve f x y
  | f x y == 0 = solve f x (y+1)
  | x >= 99, y >= 99, f (x-99) y == 1, f x (y+99) == 1, f (x-99) (y+99) == 1 = (x-99)*10000+y
  | otherwise = solve f (x+1) y
main = do
  m <- load <$> readFile "in/19.txt"
  let check x y = head $ snd $ run m 0 0 [x,y]
  print $ sum [ check x y | x <- [0..49], y <- [0..49] ] -- part A
  print $ solve check 99 0 -- part B


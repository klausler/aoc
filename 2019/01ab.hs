fuel n = if n <= 0 then 0 else n `div` 3 - 2
main = do
  input <- ((read <$>) . lines) <$> readFile "in/01.txt"
  print $ sum [ fuel n | n <- input ] -- part A
  print $ sum [ f | n <- input, f <- takeWhile (>0) $ tail $ iterate fuel n ] -- part B

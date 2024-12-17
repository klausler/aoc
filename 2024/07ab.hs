intConcat j k = read $ show j ++ show k
part1 (want:x:rest) = [ want | canMake [(+),(*)] want x rest ]
part2 (want:x:rest) = [ want | canMake [(+),(*),intConcat] want x rest ]
canMake _ want x [] = want == x
canMake fs want x (y:ys)  = or [ canMake fs want (x `f` y) ys | x <= want, f <- fs ]
main = do
  lns <- lines <$> readFile "in/07.txt"
  let tests = [ read <$> (init w1 : rest) | ln <- lns, let (w1:rest) = words ln ] :: [[Integer]]
  print $ sum $ tests >>= part1 -- part 1
  print $ sum $ tests >>= part2 -- part 2

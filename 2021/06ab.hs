import Data.List(group, sort)
step [a0,a1,a2,a3,a4,a5,a6,a7,a8] = [a1,a2,a3,a4,a5,a6,a7+a0,a8,a0]
main = do
  ln <- readFile "in/06.txt"
  let nums = read $ '[':ln ++ "]"
      cts = subtract 1 <$> length <$> (group $ sort $ [0..8] ++ nums)
      story = tail $ iterate step cts
  print $ sum $ last $ take 80 $ story -- part 1
  print $ sum $ last $ take 256 $ story -- part 2

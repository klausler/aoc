sim x (["noop"]:rest) = x : sim x rest
sim x (["addx",v]:rest) = x : x : sim (x + read v) rest
pixel p x = if abs(x-p) <= 1 then '#' else '.'
chunk [] = []
chunk xs = let (a,b) = splitAt 40 xs in a : chunk b
main = do
  cmds <- ((words <$>) . lines) <$> readFile "in/10.txt"
  print $ sum $ zipWith (*) [20, 60 .. 220] $ ((0:(sim 1 cmds)) !!) <$> [20, 60 .. 220] -- part A
  mapM_ putStrLn $ chunk $ zipWith pixel (cycle [0..39]) (take 240 $ sim 1 cmds) -- part B

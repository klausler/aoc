import Data.List(minimumBy,transpose)
chunks _ [] = []
chunks n xs = let (a,b) = splitAt n xs in a : chunks n b
((.*.) `on` f) x y = f x .*. f y
count ch = length . filter (==ch)
overlay stack = [ if (head $ dropWhile (=='2') ps) == '0' then ' ' else '#' | ps <- stack ]
main = do
  input <- init <$> readFile "in/08.txt"
  let layers = chunks (6*25) input
      partA = minimumBy (compare `on` count '0') layers
  print $ count '1' partA * count '2' partA -- part A
  sequence $ putStrLn <$> chunks 25 (overlay $ transpose layers) -- part B

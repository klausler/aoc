import Data.Function(on)
import Data.List(sortBy)
f _ [] = []
f n ("x":rest) = f (n+1) rest
f n (b:rest) = (read b, n) : f (n+1) rest
next (b,a) (x,n) = (head [ t | t <- [x, x+n..], (t+a) `mod` b == 0 ], n*b)
main = do
  txt <- readFile "in/13.txt"
  let
    line = lines txt !! 1
    fields = words $ map (\c -> if c == ',' then ' ' else c) line
    buses = reverse $ sortBy (compare `on` fst) $ f 0 fields
  print $ fst $ foldr next (0,1) buses

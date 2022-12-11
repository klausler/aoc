import Data.Char(ord)
import Data.List(transpose)
toInt ch = ord ch - ord '0'
view [] = 0
view (t:ts) = let (s,g) = span (<t) ts in length s + (if null g then 0 else 1)
main = do
  lns <- lines <$> readFile "in/08.txt"
  let input = (toInt <$>) <$> lns
      h = length input
      w = length $ head input
      input' = transpose input
      score x y = view (drop x $ input !! y) *
                  view (reverse $ take (x+1) $ input !! y) *
                  view (drop y $ input' !! x) *
                  view (reverse $ take (y+1) $ input' !! x)
  print $ maximum [ score x y | x <- [0..(w-1)], y <- [0..(h-1)] ]

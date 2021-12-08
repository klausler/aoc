import Data.Foldable(minimumBy)
((.*.) `on` f) x y = f x .*. f y
fuel2 dist = dist * (dist+1) `div` 2
scores f nums = [ (p, cost p) | p <- [minimum nums .. maximum nums] ]
  where cost p = sum [ f $ abs (n-p) | n <- nums ]
main = do
  inp <- readFile "in/07.txt"
  let nums = (read $ '[' : inp ++ "]") :: [Int]
  print $ minimumBy (compare `on` snd) $ scores id nums    -- part 1
  print $ minimumBy (compare `on` snd) $ scores fuel2 nums -- part 2

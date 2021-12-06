import Data.List(zipWith)
play p1 [] = p1
play [] p2 = p2
play (p1:p1s) (p2:p2s)
  | p1 > p2 = play (p1s ++ [p1,p2]) p2s
  | otherwise = play p1s (p2s ++ [p2,p1])
main = do
  txt <- readFile "in/22.txt"
  let
    ((_:p1s),("":_:p2s)) = break (=="") $ lines txt
    winner = play (map read p1s) (map read p2s)
  print $ sum $ zipWith (*) [1..] $ reverse winner

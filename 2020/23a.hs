import Data.List(group)
input = map read $ group "871369452"
dests n = drop (10-n) [9, 8 .. 1] ++ [9, 8 .. 1]
step (c:cs) = xs ++ (dest:take3) ++ ys ++ [c]
  where
    (take3,rest) = splitAt 3 cs
    (xs,(dest:ys)) = head [ (xs,ys) | d <- dests c, let (xs,ys) = break (==d) rest, not (null ys) ]
ans = iterate step input !! 100
main = putStrLn $ concatMap show $ tail $ take 9 $ dropWhile (/=1) $ cycle ans

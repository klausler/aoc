import Data.Bits
prog = [2,4,1,3,7,5,4,0,1,3,0,3,5,5,3,0] :: [Integer]
solve a [] = [a]
solve a (x:xs) = concat [ solve (a * 8 + j) xs | j <- [st..7], f a j == x ]
  where st = if a == 0 then 1 else 0
f a j = ((b `xor` c) `xor` 3) `mod` 8
  where
    b = j `xor` 3
    c = (a * 8 + j) `div` (2^b)
main = mapM_ print $ solve 0 $ reverse prog -- part 2

import Data.List(sort)

filename = "input01.txt"
goal = 2020

triplets values = [ (z,y,x) | x <- s, y <- s, y <= x, z <- s, z <= y, x + y + z == goal ]
  where s = sort values

multiply (x,y,z) = (x,y,z,x*y*z)

doit = fmap multiply . triplets . fmap read . lines

main = print =<< doit <$> readFile filename

import Control.Applicative((<$>))
import Data.List(sort)

filename = "in/01.txt"
goal = 2020

fastPairs goal values = search sorted $ reverse sorted
  where
    sorted = sort values
    search _ [] = []
    search [] _ = []
    search (xlist@(x:xs)) (ylist@(y:ys))
      | x + y == goal = [(x,y)]
      | x + y < goal = search xs ylist
      | otherwise = search xlist ys

multiply (x,y) = (x,y,x*y)

doit = fmap multiply . fastPairs goal . fmap read . lines

main = print =<< doit <$> readFile filename

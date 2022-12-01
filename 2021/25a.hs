import Data.List(transpose)
go ch ln = tail $ init new
  where
    new = f (last ln : (ln ++ [head ln]))
    f (x:'.':rest) | x == ch = '.' : x : f rest
    f (x:xs) = x : f xs
    f [] = []
step map = transpose $ go 'v' <$> transpose (go '>' <$> map)
toFix n f x = let x' = f x in if x == x' then n else toFix (n+1) f x'
main = print =<< toFix 1 step <$> lines <$> readFile "in/25.txt"

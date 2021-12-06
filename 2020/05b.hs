import Data.List(sort)

val 'B' = 1
val 'R' = 1
val _ = 0

codeVal xs  = cv 0 1 $ reverse xs
  where
    cv n _ [] = n
    cv n s (x:xs') = cv (n + s * val x) (2*s) xs'

missing (x:xs@(y:_))
  | x+1 == y = missing xs
  | x+2 == y = x + 1
  | otherwise = undefined

main = do
  txt <- readFile "input05.txt"
  print $ missing $ sort $ map codeVal $ lines txt

oneDup [x,y] = x == y
oneDup (x:(rest@(y:z:zs)))
  | x /= y = oneDup rest
  | y /= z = True
  | otherwise = oneDup $ dropWhile (==x) zs
oneDup _ = False
good n = oneDup digits && (and $ zipWith (>=) rest digits)
  where (digits@(_:rest)) = show n
main = print $ length $ filter good [248345..746315]

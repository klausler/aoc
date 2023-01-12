good n = (or $ zipWith (==) rest digits) && (and $ zipWith (>=) rest digits)
  where (digits@(_:rest)) = show n
main = print $ length $ filter good [248345..746315]

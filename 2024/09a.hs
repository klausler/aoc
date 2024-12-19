import Data.Char(ord)
fill need (js@((j,jct):jrest)) (ks@((k,kct):krest))
  | j >= 0, jct >= need = [(j,need)]
  | j >= 0 = (j,jct) : fill (need-jct) jrest ks
  | k < 0 = fill need js krest
  | need == m = [(k,m)]
  | jct == m, kct == m = (k,m) : fill (need-m) jrest krest
  | jct == m = (k,m) : fill (need-m) jrest ((k,kct-m) : krest)
  | otherwise = (k,m) : fill (need-m) ((j,jct-m):jrest) krest
  where m = minimum [need, jct, kct]
checksum n _ [] = n
checksum n at ((j,ct):rest) = checksum (n + j * (sum [at..at+ct-1])) (at+ct) rest
main = do
  input <- init <$> readFile "in/09.txt"
  let cts = map (\d->ord d - ord '0') input
      orig = [ (if even j then j `div` 2 else -1, ct) | (j,ct) <- zip [0..] cts ]
      total = sum [ ct | (j,ct) <- orig, j >= 0 ]
  print $ checksum 0 0 $ fill total orig $ reverse orig -- part 1

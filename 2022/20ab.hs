import Data.List(elemIndex)
doit rep len start = doit' rep 0 0 [] [ ((j,pos), j `mod` (len-1)) | (j,pos) <- zip start [0..] ]
  where doit' rep p at stack []
          | p < len = doit' rep p 0 [] $ reverse stack
          | rep > 0 = doit' (rep-1) 0 0 [] [ ((j,pos), j `mod` (len-1)) | ((j,pos),0) <- reverse stack ]
          | otherwise = reverse [ x | ((x,_),_) <- stack ]
        doit' rep p at stack ((item@(x@(_,pos),shift)):rest)
          | pos /= p = doit' rep p (at+1) (item:stack) rest
          | shift == 0 = doit' rep (p+1) (at+1) ((x,0):stack) rest
          | shift <= rem = doit' rep (p+1) at stack $ pref ++ ((x,0):suff)
          | otherwise = doit' rep p 0 [] $ ((x,shift-rem):reverse stack) ++ rest
          where rem = len - (at + 1)
                (pref,suff) = splitAt shift rest
code len final = sum [ final !! ((zeroAt + j) `mod` len) | j <- [1000, 2000, 3000] ]
  where Just zeroAt = elemIndex 0 final
main = do
  original <- ((read <$>) . lines) <$> readFile "in/20.txt"
  let len = length original
  print $ code len $ doit 0 len original -- part A
  print $ code len $ doit 9 len $ (811589153 *) <$> original -- part B

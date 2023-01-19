import Data.Char(ord)
toInt ch = ord ch - ord '0'
makeSeq n = cycle $ [0, 1, 0, -1] >>= (\v->[1..n] *> [v])
fft len xs = ((`mod` 10) . abs . sum . zipWith (*) xs . tail .  makeSeq) <$> [1..len]
partB xs = (`mod` 10) <$> (init $ scanl (flip subtract) (sum xs) xs)
main = do
  input <- init <$> readFile "in/16.txt"
  let partBoffset = read $ take 7 input
      ints = toInt <$> input
      len = length ints
      shifted = take (10000 * len - partBoffset) $ drop (partBoffset `mod` len) $ cycle ints
  print $ take 8 $ (iterate (fft $ length ints) ints) !! 100 -- part A
  print $ take 8 $ (iterate partB shifted) !! 100 -- part B

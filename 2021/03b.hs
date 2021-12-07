import Data.List(sort)
pick _ _ [ln] = sum $ fst <$> (filter snd $ zip (iterate (2*) 1) $ (=='1') <$> reverse ln)
pick f j lns = pick f (j+1) $ if f (length zeroes) (length ones) then zeroes else ones
  where
    (zeroes, ones) = span ((=='0').(!!j)) lns
main = do
  sorted <- sort <$> lines <$> readFile "in/03.txt"
  let o2 = pick (>) 0 sorted
      co2 = pick (<=) 0 sorted
  print $ o2 * co2

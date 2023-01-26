import qualified Data.Map as M
deckSize = 119315717514047 :: Integer -- prime
iters = 101741582076661
-- linear congruential function (a,b) represents: f x = (a * x + b) mod m
composeLCFs m (a,b) (c,d) = ((a*c) `mod` m, (b*c+d) `mod` m)
step ["cut",n] = (1, -(read n))
step ["deal","into",_,_] = (-1,-1)
step [_,_,_,n] = (read n, 0)
powerLCF m prod squared 0 = prod
powerLCF m prod squared n
  | odd n = powerLCF m (composeLCFs m prod squared) (composeLCFs m squared squared) (n `div` 2)
  | otherwise = powerLCF m prod (composeLCFs m squared squared) (n `div` 2)
powerMod m prod squared 0 = prod
powerMod m prod squared n
  | odd n = powerMod m ((prod * squared) `mod` m) ((squared * squared) `mod` m) (n `div` 2)
  | otherwise = powerMod m prod ((squared * squared) `mod` m) (n `div` 2)
invertMod m n = powerMod m 1 n $ m - 2 -- due to Fermat's little theorem & m being prime
main = do
  input <- ((words <$>) . lines) <$> readFile "in/22.txt"
  let lcf = foldl (composeLCFs deckSize) (1,0) $ step <$> input
      lcfPower = powerLCF deckSize (1,0) lcf iters
  print $ ((2020 - snd lcfPower) * (invertMod deckSize $ fst lcfPower)) `mod` deckSize

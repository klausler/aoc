import Data.Char(chr, ord)
import Data.List(findIndex, group, sort, permutations)
seqs = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg" ]
decode perm wd = sort [ chr (ord 'a' + (perm !! (ord ch - ord 'a'))) | ch <- wd ]
digitValue perm o = let Just j = findIndex (== decode perm o) seqs in j
solve ins = head [ p | p <- permutations [0..6], all ((`elem` seqs) . decode p) ins ]
outValue ins outs = sum $ zipWith (*) (iterate (10*) 1) $ reverse $ digitValue (solve ins) <$> outs
doit wds = outValue (take 10 wds) (drop 11 wds)
main = do
  lns <- lines <$> readFile "in/08.txt"
  print $ sum $ doit <$> words <$> lns

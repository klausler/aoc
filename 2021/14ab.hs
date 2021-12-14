import Data.List(sort)
import qualified Data.Map as M
pairs [_] = []
pairs (a:(rest@(z:_))) = (a,z) : pairs rest
step rules m = M.fromListWith (+) [ (to,n) | (k,n) <- M.toList m, to <- M.findWithDefault [k] k rules ]
score polymer final = maximum letterCounts - minimum letterCounts
  where
    lCounts = concat [ [(a,n), (z,n)] | ((a,z),n) <- M.toList final ]
              ++ [(head polymer, 1), (last polymer, 1)]
    letterCounts = (`div` 2) <$> (M.elems $ M.fromListWith (+) lCounts)
main = do
  (polymer:"":lns) <- lines <$> readFile "in/14.txt"
  let rules = M.fromList [ ((a,z), [(a,to), (to,z)]) | (a:z:' ':'-':'>':' ':to:"") <- lns ]
      pairCounts = M.fromListWith (+) [ (p,1) | p <- pairs polymer ]
      story = iterate (step rules) pairCounts
  print $ score polymer $ story !! 10 -- part 1
  print $ score polymer $ story !! 40 -- part 2

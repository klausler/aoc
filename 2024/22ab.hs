import Data.Bits(xor)
import qualified Data.Map as M
import qualified Data.Set as S
mix = xor
prune = (`mod` 16777216)
next n = prune $ mix n2 (2048 * n2)
  where n1 = prune $ mix n (64 * n)
        n2 = prune $ mix n1 (n1 `div` 32)
diffs [_] = []
diffs (x:(rest@(y:_))) = (y-x) : diffs rest
f a b c d = 19*19*19*(a+9) + 19*19*(b+9) + 19*(c+9) + (d+9)
blocks (a:(rest@(b:c:d:_))) (v:vs) = (f a b c d, v) : blocks rest vs
blocks _ _ = []
getBlocks n = blocks ds $ drop 4 vals
  where vals = (`mod` 10) <$> iterate next n
        ds = diffs $ take 2001 vals
makeBMaps nums = (M.fromListWith (flip const) . getBlocks) <$> nums
score bmaps k = sum $ M.findWithDefault 0 k <$> bmaps
main = do
  lns <- lines <$> readFile "in/22.txt"
  let nums = read <$> lns :: [Int]
      bmaps = makeBMaps nums
      keys = S.toList $ S.unions $ (S.fromList . M.keys) <$> bmaps
  print $ sum [ iterate next n !! 2000 | n <- nums ] -- part 1
  print $ maximum $ score bmaps <$> keys -- part 2

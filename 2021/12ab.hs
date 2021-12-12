import Data.Char(isAsciiLower)
import qualified Data.Map as M
import qualified Data.Set as S
paths _ _ _ "end" = [["end"]]
paths links canRepeat dead at@(at1:_)
  | canRepeat, at `S.member` dead, at /= "start" = (at:) <$> (next >>= paths links False dead)
  | at `S.member` dead = []
  | otherwise = (at:) <$> (next >>= paths links canRepeat dead')
  where next = S.toList $ M.findWithDefault S.empty at links
        dead' =  if isAsciiLower at1 then at `S.insert` dead else dead
main = do
  lns <- lines <$> readFile "in/12.txt"
  let links = M.fromListWith S.union $ concat
        [ [(from, S.singleton to'), (to', S.singleton from)]
        | (from, to) <- break (=='-') <$> lns, let to' = tail to ]
  print $ length $ paths links False S.empty "start" -- part 1
  print $ length $ paths links True S.empty "start" -- part 2

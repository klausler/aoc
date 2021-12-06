import Data.List(zipWith)
import qualified Data.Set as S
play _ p1 [] = (True, p1)
play _ [] p2 = (False, p2)
play hist (p1@(p1c:p1s)) (p2@(p2c:p2s))
  | (p1,p2) `S.member` hist = (True, [] {-unused-})
  | p1c <= length p1s, p2c <= length p2s = if recurse then p1round else p2round
  | p1c > p2c = p1round
  | otherwise = p2round
  where
    next = play $ S.insert (p1,p2) hist
    p1round = next (p1s ++ [p1c,p2c]) p2s
    p2round = next p1s (p2s ++ [p2c,p1c])
    recurse = fst $ play S.empty (take p1c p1s) (take p2c p2s)
main = do
  txt <- readFile "input22.txt"
  let
    ((_:x),("":_:y)) = break (=="") $ lines txt
  print $ sum $ zipWith (*) [1..] $ reverse $ snd $ play S.empty (map read x) (map read y)

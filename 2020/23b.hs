import Data.List(group)
import qualified Data.Map.Strict as M
big = 1000000
myInput = 871369452
input = (map read $ group $ show myInput) ++ [10]
dests n = let n' = if n == 1 then big else n - 1 in n' : dests n'
after map at = M.findWithDefault (at+1) at map
mseq map at = at : mseq map (after map at)
ins k x m
  | x == k+1 = M.delete k m
  | otherwise = M.insert k x m
toMap [_] = M.empty
toMap (x:(rest@(y:_))) = ins x y $ toMap rest
makeMap = ins big (head input) $ toMap input
step (m,hd)
  | dest == hd = (m,x)
  | otherwise = (m',hd')
  where
    (_:x:y:z:hd':_) = mseq m hd
    ok w = w /= x && w /= y && w /= z
    dest = head $ filter ok $ dests hd
    m' = ins hd hd' $ ins dest x $ ins z (after m dest) m
ans = fst $ iterate step (makeMap, head input) !! 10000000
main = print $ product $ tail $ take 3 $ mseq ans 1

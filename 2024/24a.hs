import Data.Bits
import qualified Data.Map as M
gate m op a b
  | op == "AND" = (m M.! a)  .&.  (m M.! b)
  | op == "OR"  = (m M.! a)  .|.  (m M.! b)
  | op == "XOR" = (m M.! a) `xor` (m M.! b)
fromBits n [] = n
fromBits n (b:bs) = fromBits (2 * n + b) bs
twoDig n
  | n < 10 = '0' : show n
  | otherwise = show n
main = do
  lns <- lines <$> readFile "in/24.txt"
  let (wires,(_:gates)) = break null lns
      gwds = words <$> gates
      m = M.fromList $
        [ (init n, read v) | w <- wires, let [n,v] = words w ] ++
        [ (res, gate m op a b) | [a,op,b,_,res] <- gwds ] :: M.Map String Int
      zs = filter (('z'==).head) $ M.keys m
  print $ fromBits 0 $ reverse $ map (m M.!) zs -- part 1

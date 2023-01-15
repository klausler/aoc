import qualified Data.Map as M
import qualified Data.Set as S
keep ch = ch == ' ' || ch >= '0' && ch <= '9' || ch >= 'A' && ch <= 'Z'
amounts [] = []
amounts (item:n:rest) = (item, read n) : amounts rest
topoOrder m set
  | null next = []
  | otherwise = next ++ topoOrder m (S.union set $ S.fromList next)
  where next = [ item | (item,(ct,needs)) <- M.assocs m
                      , S.notMember item set
                      , all (`S.member` set) $ fst <$> needs ]
accumulate _ bill [] = bill M.! "ORE"
accumulate m bill (item:rest) = accumulate m bill' rest
  where (ct,needs) = m M.! item
        bi = M.findWithDefault 0 item bill
        times = (bi + ct - 1) `div` ct
        bill' = M.unionWith (+) bill $ M.fromList [ (item', times * amt) | (item',amt) <- needs ]
oreFor m s fuel = accumulate m (M.singleton "FUEL" fuel) s
search f lo hi
  | lo + 1 >= hi = lo
  | f mid = search f lo mid
  | otherwise = search f mid hi
  where mid = lo + (hi - lo) `div` 2
main = do
  input <- lines <$> readFile "in/14.txt"
  let lns = (reverse . words . filter keep) <$> input
      m = M.fromList [ (to,(read ct, amounts need)) | (to:ct:need) <- lns ]
      s = reverse $ topoOrder m $ S.singleton "ORE"
      lo = last $ takeWhile ((<1000000000000) . oreFor m s) $ (2^) <$> [0..]
  print $ oreFor m s 1 -- part A
  print $ search (\f->oreFor m s f > 1000000000000) lo $ 2 * lo -- part B

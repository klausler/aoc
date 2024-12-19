import qualified Data.Map as M
magic 0 = [1]
magic n | even len = let (p,s) = splitAt (len `div` 2) digs
                     in [ read p, read s ]
  where
    digs = show n
    len = length digs
magic n = [n * 2024]
blink m = M.fromListWith (+) [ (n',ct) | (n,ct) <- M.assocs m , n' <- magic n ]
main = do
  [ln] <- lines <$> readFile "in/11.txt"
  let nums = read <$> words ln
      map0 = M.fromListWith (+) [ (n,1) | n <- nums ]
  print $ sum $ M.elems $ (iterate blink map0) !! 25 -- part 1
  print $ sum $ M.elems $ (iterate blink map0) !! 75 -- part 2

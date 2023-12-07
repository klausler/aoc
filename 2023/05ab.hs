chunked [] = []
chunked ("":rest) = chunked rest
chunked lns = let (chunk, rest) = break null lns in chunk : chunked rest
pairUp [] = []
pairUp (x:y:rest) = (x,y) : pairUp rest
applyMap _ (x,n) | n <= 0 = []
applyMap [] xn = [xn]
applyMap ([to,from,n]:rest) (x,xn)
  | start < end = [(to+start-from, end-start)] ++
                  applyMap rest (x,from-x) ++ applyMap rest (fend, xend-fend)
  | otherwise = applyMap rest (x,xn)
  where
    fend = from + n
    xend = x + xn
    start = max x from
    end = min fend xend
main = do
  lns <- lines <$> readFile "in/05.txt"
  let ((seedLine:_):mapChunks) = chunked lns
      seeds = read <$> (tail $ words seedLine)
      maps = (fmap (fmap read . words) . tail) <$> mapChunks
      applyMaps ivals = foldl f ivals maps where f ivals m = ivals >>= applyMap m
  print $ minimum $ fst <$> applyMaps ((,1) <$> seeds) -- part A
  print $ minimum $ fst <$> applyMaps (pairUp seeds) -- part B

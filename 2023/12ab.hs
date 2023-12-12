import Data.Array
suffices [] = [[]]
suffices (list@(_:rest)) = list : suffices rest
solve codes sizes = dynamic ! (0,0)
  where
    dynamic = listArray ((0,0),(length codes, length sizes))
      [ ways (j,k) cs ss | (j,cs) <- zip [0..] $ suffices codes
                         , (k,ss) <- zip [0..] $ suffices sizes ]
    ways _ "" [] = 1
    ways _ "" _ = 0
    ways (j,k) ('.':_) _ = dynamic ! (j+1,k)
    ways (j,k) ('?':rest) sizes = ways (j,k) ('.':rest) sizes + ways (j,k) ('#':rest) sizes
    ways _ ('#':_) [] = 0
    ways (j,k) (str@('#':_)) (sz:sizes)
      | length pref < sz = 0
      | any (=='.') pref = 0
      | ('#':_) <- suff = 0
      | otherwise = dynamic ! (if null suff then j+sz else j+sz+1,k+1)
      where (pref,suff) = splitAt sz str
solveB codes sizes = solve (concat $ take 9 $ cycle [codes, "?"])
                           (concat $ take 5 $ cycle [sizes])
main = do
  lns <- lines <$> readFile "in/12.txt"
  let parsed = [ (codes, read ('[':sizes++"]")) | ln <- lns, let [codes, sizes] = words ln ]
  print $ sum [ solve codes sizes | (codes,sizes) <- parsed ] -- part A
  print $ sum [ solveB codes sizes | (codes,sizes) <- parsed ] -- part B

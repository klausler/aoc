import qualified Data.Set as S
toDir rows cols '^' = [\(r,c)->(if r == 1 then rows else r-1,c)]
toDir rows cols '>' = [\(r,c)->(r,if c == cols then 1 else c+1)]
toDir rows cols 'v' = [\(r,c)->(if r == rows then 1 else r+1,c)]
toDir rows cols '<' = [\(r,c)->(r,if c == 1 then cols else c-1)]
toDir _ _ _ = []
moves (r,c) = [(r,c), (r-1,c), (r,c+1), (r+1,c), (r,c-1)]
bfs rows cols goal n bzs pts
  | S.member goal pts = (n,bzs)
  | otherwise = bfs rows cols goal (n+1) bzs' pts'
  where bzs' = [ (f (r,c), f) | ((r,c),f) <- bzs ]
        bzs'' = S.fromList [ f (r,c) | ((r,c),f) <- bzs ]
        pts' = S.fromList $ filter ok $ S.toList pts >>= moves
        ok (r,c) = (r > 0 || (r == 0 && c == 1)) &&
                   (r <= rows || (r == rows + 1 && c == cols)) &&
                   c > 0 && c <= cols && S.notMember (r,c) bzs''
main = do
  lns <- lines <$> readFile "in/24.txt"
  let rows = length lns - 2
      cols = length (head lns) - 2
      blizzards = [ ((r,c),f) | (r,ln) <- zip [0..] lns, (c,ch) <- zip [0..] ln,
                                f <- toDir rows cols ch ]
      (n1,bzs1) = bfs rows cols (rows+1,cols) 0 blizzards $ S.singleton (0,1)
      (n2,bzs2) = bfs rows cols (0,1) n1 bzs1 $ S.singleton (rows+1,cols)
      (n3,bzs3) = bfs rows cols (rows+1,cols) n2 bzs2 $ S.singleton (0,1)
  print n1 -- part A
  print n3 -- part B

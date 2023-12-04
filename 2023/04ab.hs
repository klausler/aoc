import qualified Data.Set as S
parse :: String -> ([Int],[Int])
parse card = (read <$> words wins, read <$> words nums)
  where
    (_,card') = break (==':') card
    (wins,card'') = break (=='|') $ tail card'
    nums = tail card''
numWins (wns,ns) = S.size $ S.intersection (S.fromList wns) (S.fromList ns)
partAValue w = if w == 0 then 0 else 2^(w-1)
copies _ [] = []
copies (n:ns) (w:ws) = n : copies ns' ws
  where (inc,rest) = splitAt w ns
        ns' = ((n+) <$> inc) ++ rest
main = do
  lns <- lines <$> readFile "in/04.txt"
  let cardWins = numWins <$> parse <$> lns
  print $ sum $ partAValue <$> cardWins -- part A
  print $ sum $ copies (repeat 1) cardWins -- part B

import qualified Data.Map as M
suffix "" rest = [rest]
suffix (x:xs) (y:ys) | x == y = suffix xs ys
suffix _ _ = []
possible _ memo at "" = (1, memo)
possible _ memo at _ | Just n <- M.lookup at memo = (n, memo)
possible pats memo at str = tryPats pats memo 0
  where
    tryPats [] memo ct = (ct, M.insert at ct memo)
    tryPats (p:ps) memo ct | [suff] <- suffix p str =
      let (n,memo') = possible pats memo (at+length p) suff
      in tryPats ps memo' $ n + ct
    tryPats (_:ps) memo ct = tryPats ps memo ct
main = do
  (l1:"":goals) <- lines <$> readFile "in/19.txt"
  let pats = init <$> words (l1++",")
      ways = fst <$> possible pats M.empty 0 <$> goals
  print $ length $ filter (>0) ways -- part 1
  print $ sum ways -- part 2

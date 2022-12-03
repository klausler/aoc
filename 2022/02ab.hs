import Data.Char(ord)
which ch
  | ch >= 'A', ch <= 'C' = ord ch - ord 'A'
  | ch >= 'X', ch <= 'Z' = ord ch - ord 'X'
parse ln = let [[x],[y]] = words ln in (which x, which y)
rsp them me = 3 * ((me - them + 1) `mod` 3)
turn1 (them, me) = me + 1 + rsp them me
turn2 (them, outcome) = mine + 1 + 3 * outcome
  where mine = (them + outcome - 1) `mod` 3
main = do
  parsed <- ((parse <$>) . lines) <$> readFile "in/02.txt"
  print $ sum $ turn1 <$> parsed -- part A
  print $ sum $ turn2 <$> parsed -- part B

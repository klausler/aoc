import Data.Char(ord)
prio ch
  | ch >= 'a', ch <= 'z' = ord ch - ord 'a' + 1
  | ch >= 'A', ch <= 'Z' = ord ch - ord 'A' + 27
partA ln = let (a,b) = splitAt (length ln `div` 2) ln in prio $ head $ filter (`elem` b) a
partB [] = 0
partB (a:b:c:rest) = (prio $ head $ [ch | ch<-a, ch `elem` b, ch `elem` c]) + partB rest
main = do
  input <- lines <$> readFile "in/03.txt"
  print $ sum $ partA <$> input
  print $ partB input

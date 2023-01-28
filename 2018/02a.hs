import Data.List(group,sort)
main = do
  input <- ((group . sort) <$>) . lines <$> readFile "in/02.txt"
  let has2 = length $ filter (any ((==2) . length)) input
      has3 = length $ filter (any ((==3) . length)) input
  print $ has2 * has3 -- part A

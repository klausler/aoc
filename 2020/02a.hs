import Data.Char(isDigit)
check ln = hits >= read n1 && hits <= read n2
  where
    (n1,r1) = span isDigit ln
    ('-':r2) = r1
    (n2,' ':letter:':':' ':pw) = span isDigit r2
    hits = length $ filter (==letter) pw
main = do
  text <- readFile "in/02.txt"
  let ans = filter check $ lines $ text
  mapM_ putStrLn ans
  print $ length ans



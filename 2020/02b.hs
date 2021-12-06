import Data.Char(isDigit)
check ln = length [ () | n <- posns, letter == pw !! n] == 1
  where
    (n1,r1) = span isDigit ln
    ('-':r2) = r1
    (n2,' ':letter:':':' ':pw) = span isDigit r2
    posns = [ read n - 1 | n <- [n1, n2] ]
main = do
  text <- readFile "in/02.txt"
  let ans = filter check $ lines $ text
  mapM_ putStrLn ans
  print $ length ans



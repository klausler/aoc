import IntCode
main = do
  m <- load <$> readFile "in/09.txt"
  let (_,[partA]) = run m 0 0 [1]
      (_,[partB]) = run m 0 0 [2]
  sequence $ print <$> [partA, partB]

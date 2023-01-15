import IntCode
import Data.List(permutations)
main = do
  m <- load <$> readFile "in/07.txt"
  let runAmp ph input = snd $ run m 0 0 (ph:input)
      partA ph input = let [output] = runAmp ph [input] in output
      partB phases = let out = foldl1 (.) (runAmp <$> phases) $ 0 : out in last out
  print $ maximum [ foldl1 (.) (partA <$> p) 0 | p <- permutations [0..4] ] -- part A
  print $ maximum [ partB p | p <- permutations [5..9] ] -- part B

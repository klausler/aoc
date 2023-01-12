import IntCode
import qualified Data.Map as M
main = do
  m <- load <$> readFile "in/05.txt"
  let [partA] = dropWhile (==0) $ snd $ run m 0 [1]
      [partB] = dropWhile (==0) $ snd $ run m 0 [5]
  sequence $ print <$> [partA, partB]

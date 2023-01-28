import IntCode
import Data.Char(chr,ord)
main = do
  m <- load <$> readFile "in/25.txt"
  interact ((chr <$>) . snd . run m 0 0 . (ord <$>))

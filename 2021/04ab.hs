import Data.List(partition, transpose)
import Control.Monad(join)
getBoards [] = []
getBoards lines = board : getBoards rest
  where
    ((_:took), rest) = splitAt 6 lines
    board = (read <$>) <$> words <$> took
isAnyRowDone b = or $ (all (<0)) <$> b
isSolved bd = isAnyRowDone bd || isAnyRowDone (transpose bd)
play num bd = (rep <$>) <$> bd
  where rep n = if n == num then -1 else n
score = sum . filter (>=0) . join
game [] _ = []
game bds (n:ns) = scores : game left ns
  where
    (done, left) = partition isSolved $ play n <$> bds
    scores = ((n*).score) <$> done
main = do
  (l1:rest) <- lines <$> readFile "in/04.txt"
  let nums = (read $ '[':l1++"]")
      bds = (getBoards rest)
  print $ [head, last] <*> [join $ game bds nums]

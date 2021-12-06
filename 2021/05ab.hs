import Data.Char(isDigit)
import Data.List(group, sort)
onlyDigit c = if isDigit c then c else ' '
nonDiag [r1,c1,r2,c2] = r1 == r2 || c1 == c2
doUntil pred next x = x : if pred x then [] else doUntil pred next (next x)
expand [r1,c1,r2,c2] = doUntil done next (r1,c1)
  where done (r,c) = r == r2 && c == c2
        next (r,c) = (r + signum (r2-r1), c + signum (c2-c1))
countMultipleHits items = length $ filter ((1<).length) $ group $ sort $ expand =<< items
main = do
  lns <- lines <$> readFile "in/05.txt"
  let items = ((read <$>) . words . (onlyDigit <$>)) <$> lns
  print $ countMultipleHits $ filter nonDiag items -- part 1
  print $ countMultipleHits items -- part 2

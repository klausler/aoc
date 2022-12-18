import Data.Bits
import qualified Data.Map as M
rocks = [ [30], [8,28,8], [4,4,28], [16,16,16,16], [24,24] ]
doShift rock '<' = if all (<64) rock then (`shiftL` 1) <$> rock else rock
doShift rock '>' = if all even rock then (`shiftR` 1) <$> rock else rock
leftRight rock pile ch
  | not $ any (/=0) $ zipWith (.&.) shifted pile = shifted
  | otherwise = rock
  where shifted = doShift rock ch
descend :: ([Int],String,Int) -> [Int] -> ([Int],String,Int)
descend (pile,(lr:lrs),lrCt) rock
  | length pushed == length hits
  , not $ any (/=0) hits = descend (pile,lrs,lrCt+1) pushed
  | otherwise = (dropWhile (==0) merged, lrs, lrCt+1)
  where shifted = leftRight rock pile lr
        pushed = 0 : shifted
        hits = zipWith (.&.) pushed pile
        (top,rest) = splitAt (length shifted) pile
        merged = zipWith (.|.) shifted top ++ rest
doRock (pile,lrs,lrCt) rock = descend (replicate (length rock + 3) 0 ++ pile, lrs, lrCt) rock
doRocks lrs rocks n pile = foldl doRock (pile, lrs, 0) $ take n rocks
pileMatch _ _ 127 = True
pileMatch [] [] _ = True
pileMatch [] _ _ = False
pileMatch _ [] _ = False
pileMatch (x:xs) (y:ys) orSum
  | x == y = pileMatch xs ys (orSum .|. x)
  | otherwise = False
findCycle lrCt lrs lrAt (rocks@(rock:rocks')) rockAt map pile
  | Just (pile0,lrAt0,rockAt0) <- M.lookup key map
  , pileMatch pile pile0 0 = (pile0,rockAt0,pile,rockAt,lrs,rocks)
  | otherwise = findCycle lrCt lrs' lrAt' rocks' (rockAt+1) map' pile'
  where key = (lrAt `mod` lrCt, rockAt `mod` 4)
        (pile',lrs',lrAt') = doRock (pile,lrs,lrAt) rock
        map' = M.insert key (pile,lrAt,rockAt) map
main = do
  input <- init <$> readFile "in/17.txt"
  let (partA, _, _) = doRocks (cycle input) (cycle rocks) 2022 []
      (pile0,n0,pile1,n1,lrs',rocks') =
        findCycle (length input) (cycle input) 0 (cycle rocks) 0 M.empty []
      (d,m) = (1000000000000 - n1) `divMod` (n1-n0)
      (partB, _, _) = doRocks lrs' rocks' m pile1
  print $ length partA  -- part A
  print $ length partB + d * (length pile1 - length pile0) -- part B

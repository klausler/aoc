import Data.Array
import Data.Char(chr, ord)
import Data.List(findIndex)
neighbors arr (r,c) = [ (r',c') | r' <- [r-1 .. r+1], c' <- [c-1 .. c+1]
                                , r' >= 1, r' <= n, c' >= 1, c' <= m, (r',c') /= (r,c) ]
  where ((1,1),(n,m)) = bounds arr
flash arr
  | null new = arr
  | otherwise = flash $ foldr inc arr incrs // ((\i->(i,0)) <$> new)
  where
    new = [ i | (i,e) <- assocs arr, e > 9 ]
    incrs = [ i | i <- new >>= neighbors arr, arr ! i > 0 ]
    inc i arr = arr // [(i, arr ! i + 1)]
main = do
  lns <- lines <$> readFile "in/11.txt"
  let n = length lns
      m = length $ head lns
      arr = listArray ((1,1),(n,m)) $ concatMap ((subtract (ord '0') . ord) <$>) lns
      story = iterate (flash . ((+1) <$>))  arr
  print $ sum $ (length . filter (==0) . elems) <$> (take 101 story) -- part 1
  print $ findIndex (all (==0) . elems) story -- part 2

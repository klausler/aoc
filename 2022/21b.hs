import qualified Data.Map as M
val m [j] h = read j
val m [x,"+",y] h = m x h + m y h
val m [x,"-",y] h = m x h - m y h
val m [x,"*",y] h = m x h * m y h
val m [x,"/",y] h = m x h `div` m y h
rootVal m [x,_,y] h = m x h - m y h
binSearch try lo hi
  | xMid == 0 = mid
  | xLo < xHi, xMid < 0 = binSearch try mid hi
  | xLo > xHi, xMid > 0 = binSearch try mid hi
  | otherwise = binSearch try lo mid
  where xLo = try lo
        xHi = try hi
        mid = lo + ((hi - lo) `div` 2)
        xMid = try mid
main = do
  input <- lines <$> readFile "in/21.txt"
  let mB = M.fromList [ (init name,
                         if name == "humn:" then id
                         else if name == "root:" then rootVal (mB M.!) rest
                         else val (mB M.!) rest) | (name:rest) <- words <$> input ]
  print $ binSearch (mB M.! "root") (-1000000000000000) 1000000000000000 -- part B

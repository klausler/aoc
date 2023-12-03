import Data.Array
import Data.Char(digitToInt, isDigit)
import qualified Data.Map as M
import qualified Data.Set as S
get a (at@(r,c))
  | not (isDigit $ a!(r,c-1)), not (null digits), any (isPart . (a!)) totalHalo =
    [(read $ (a!) <$> digits, adjGears)]
  | otherwise = []
  where
    isPart c = not (isDigit c) && c /= '.'
    digits = takeWhile (isDigit . (a!)) [ (r,c+j) | j <- [0..] ]
    halo (r,c) = [ (r+j,c+k) | j<-[-1,1], k<-[-1..1] ]
    totalHalo = [(r,c-1), (r,c+length digits)] ++ (digits >>= halo)
    adjGears = filter ((=='*').(a!)) $ S.toList $ S.fromList totalHalo
main = do
  lns <- lines <$> readFile "in/03.txt"
  let (rows,cols) = (length lns, maximum $ (length <$> lns))
      pad ln = '.' : (take (cols+1) $ ln ++ repeat '.')
      bordered = pad <$> ("":(lns++[""]))
      arr = listArray ((0,0),(rows+1,cols+1)) $ bordered >>= id
      scanned = [(r,c)|r<-[1..rows],c<-[1..cols]] >>= get arr
      gearNeighbors = M.fromListWith (++) [(g,[n]) | (n,gs)<-scanned, g<-gs]
  print $ sum $ fst <$> scanned -- part A
  print $ sum $ [ n*m | (_,[n,m]) <- M.toList gearNeighbors ] -- part B
